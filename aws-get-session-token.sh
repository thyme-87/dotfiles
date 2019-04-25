#!/bin/bash

#IMPORTANT:
# AccessKeyId is NOT identical with the AccessKeyID provided in ~/.aws/credentials
# SecretAccessKeyis NOT identical with the AccessKeyID provided in ~/.aws/credentials
# so what we need to do is to use environment variables as they take precedence over profile credentials
# the existing AWS_PROFILE does not need to be unset



#TODO improve support for aws sts get-session-token --duration-seconds [value] (use -d [time in seconds])
# --help
# aws-get-session-token 123456 [session duration in seconds]

function __aws_get_session_token {
    local AWS_CONFIG_FILE="${HOME}/.aws/config"
    local MFA_TOKEN=""
    local MFA_TOKEN_ID=""

    #trap ctrl-c, restore tty and quit
    trap 'stty echo && trap - INT && return 1' INT

    if [[ ! -z $AWS_PROFILE ]]
    then
        echo "Attempting to obtain sts credentials for role/profile <${AWS_PROFILE}>"
    else
        echo "\$AWS_PROFILE not set."
        return 1
    fi

    if [[ -f "${AWS_CONFIG_FILE}" ]]
    then
        MFA_TOKEN_ID=$(sed -nr "/^\[profile ${AWS_PROFILE}\]/ { :l /^mfa_serial[ ]*=/ { s/.*=[ ]*//; p; q;}; n; b l;}" ${AWS_CONFIG_FILE})
        if [ $? -eq 0 ] && [[ ${MFA_TOKEN_ID} != "" ]]
        then
            #Block to get MFA-TOKEN
            while [[ ! "${MFA_TOKEN}" =~ ^[0-9]{6}$ ]]
            do
                if [[ ! -z "$1" ]] && [[ "$1" =~ ^[0-9]{6}$ ]]
                then
                    MFA_TOKEN=$1
                    break
                elif [[ "${MFA_TOKEN}" =~ ^q+ ]]
                then
                    echo "Quitting"
                    return 1
                else
                    echo "Please enter a valid MFA-TOKEN (type q to quit):"
                    stty -echo
                    read MFA_TOKEN
                    stty echo
                fi
            done
            #END Block to get MFA-TOKEN
        else
            echo "Could not find mfa_serial for profile <${AWS_PROFILE}> in <${AWS_CONFIG_FILE}>"
            echo 'Try "aws iam list-mfa-devices" to check if an mfa-device is configured for your account/profile and you have read access for the mfa-device'
            unset AWS_ACCESS_KEY_ID AWS_SECRET_ACCESS_KEY AWS_SESSION_TOKEN
            return 1
            fi
        else
            echo "Could not find file: ${AWS_CONFIG_FILE}"
            return 1
        fi

    #unset existing environment variables to make sure that we initiate a new session, regardless if there is an existing (valid) session
    [[ -z "${AWS_SECRET_ACCESS_KEY_ID}" || "${AWS_ACCESS_KEY_ID}" || "${AWS_SESSION_TOKEN}" ]] && unset AWS_ACCESS_KEY_ID AWS_SECRET_ACCESS_KEY AWS_SESSION_TOKEN


    if [[ ! -z "$2" ]] && [[ "$2" =~ ^[0-9]{3,}$ ]] && [[ "$2" > 899 ]]
        then
            echo "Requesting temporary credentials... (session duration: ${2} seconds)"
            RESULT=$(aws sts get-session-token --serial-number $MFA_TOKEN_ID --token-code $MFA_TOKEN --duration-seconds $2)
        else
            echo "Requesting temporary credentials..."
            RESULT=$(aws sts get-session-token --serial-number $MFA_TOKEN_ID --token-code $MFA_TOKEN)
    fi

    if [ $? -eq 0 ] || [ "${RESULT}" == 0 ]
    then
        #strip quotes and commas
        RESULT=$(sed -e 's/\"//g' -e 's/,//g' <<< "${RESULT}")

        ACCESS_KEY_ID=$(awk '/AccessKeyId/{print $2}' <<< "${RESULT}")
        SECRET_ACCESS_KEY=$(awk '/SecretAccessKey/{print $2}' <<< "${RESULT}")
        SESSION_TOKEN=$(awk '/SessionToken/{print $2}' <<< "${RESULT}")
        TOKEN_EXPIRATION=$(awk '/Expiration/{print $2}' <<< "${RESULT}")

        #clear
        echo "Success! Session will expire at: $TOKEN_EXPIRATION"

        #export credentials to environment
        export AWS_ACCESS_KEY_ID="${ACCESS_KEY_ID}" AWS_SECRET_ACCESS_KEY="${SECRET_ACCESS_KEY}" AWS_SESSION_TOKEN="${SESSION_TOKEN}"
        return 0
    else #FAIL
        unset AWS_ACCESS_KEY_ID AWS_SECRET_ACCESS_KEY AWS_SESSION_TOKEN
        return 1
    fi
}
