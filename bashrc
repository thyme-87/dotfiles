#
# ~/.bashrc
# This file is executed for interactive non-login shells.
#
#TODO convenience for setting the card-profile when switching between laptop mode and docked mode
# pactl set-card-profile alsa_card.pci-0000_00_1f.3 "output:hdmi-stereo-extra1"

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

#-------------- ALIAS BEGIN----------------
alias tor='~/tor-browser_en-US/start-tor-browser --detach'
alias bopdf='~/Dropbox/BerlinOnline/4_other/bo_pdf/bopdf.sh'
alias mychromium='setsid /usr/bin/chromium >& /dev/null &'
alias csv-view='__csv_view'
alias b64decode='__base64_decode'
alias ll='ls -l'
alias gnome-settings='XDG_CURRENT_DESKTOP=GNOME gnome-control-center'

#if gnome-settings isn't working, reset current "pane"(?) via dconf
#TODO this should be part of system setup
alias fix-gnome-lockscreen='sudo vim /etc/gdm/custom.conf'
alias gitp='git push'
alias cal='cal -w'
alias nextmonths='cal -3wm $(expr $(date +%m) + 1) $(date +%Y)'
alias get_audio_sources="pacmd list-sinks | grep -e 'name:' -e 'index:'"
alias set_default_audio_sink="pactl set-default-sink {}"

alias ansdoc='__ansdoc'

function __ansdoc {
    if [ "${1}X" != "X" ]; then
        ansible-doc "${1}"
    else
      PYTHON_VERSION=$(python --version | cut -d ' ' -f 2 | grep -Po '\d+.\d+')
      ansible-doc $(rg \
      -g "!test/" \
      -g "!tests/" \
      -g "!module_utils/" \
      -g "!doc_fragments/" \
      -g "!cliconf/" \
      -g "!terminal/" \
      -g "!action/" \
      -g "!httpapi/" \
      -g "!callback/" \
      --iglob "*.py" \
      --iglob "!__init__.py" \
      --files  \
      "${HOME}/.ansible/collections/ansible_collections" \
      "${HOME}/.local/lib/python${PYTHON_VERSION}/site-packages/ansible" \
      "${HOME}/.local/lib/python${PYTHON_VERSION}/site-packages/ansible_collections" \
      | grep -oP "^.*(collections/|ansible/modules/)\K.*" \
      | sed -e 's!plugins/modules/!!' -e 's!/!.!g' -e 's!\.py!!' \
      | fzf --preview 'ansible-doc {}' -e)
    fi
}

#update all pip packages
alias pip-update-all="pip list --outdated --format=freeze | grep -v '^\-e' | cut -d = -f 1  | xargs -n1 pip install -U"

#convenience for fzf
alias lookup="fzf --preview 'highlight -O ansi -l {}'"
alias vimup="vim \$(lookup)"
alias vimfzf="vimup"

alias aws_list_instance_names="aws ec2 describe-instances | jq '.[][].Instances[].Tags[] | {Instance_Name: select(.Key==\"Name\").Value}'"

# make ls use colors automatically
alias ls='ls --color'
alias grep='grep --color'
#-------------- ALIAS END -----------------

function __base64_decode {
    echo $(base64 --decode <<< "${1}")
}

function __csv_view {
    column -s, -t $1 | less -#2 -N -S
}

source ~/dotfiles/bin/git-prompt.sh


export EDITOR="vim"
export JAVA_HOME=/usr/bin/java
export LC_ALL=en_US.UTF-8
#export XDG_CURRENT_DESKTOP=GNOME


#PATH=$PATH:~/bin #local bin for personal shell scripts
export PATH

#----------------------------[SETTINGS FOR GO BEGIN]-----------------------------------
#Settings for GO development
#requires /home/$USER/golib and /home/$USER/gocode to be present
#adds both paths to PATH
#export GOROOT=/usr/local/go
export GO111MODULE=auto
export GOPATH=/home/$USER/golib:/home/$USER/gocode
export PATH=$PATH:$GOROOT/bin:$GOPATH/bin
#----------------------------[SETTINGS FOR GO END]-----------------------------------

#indication via ~/dotfiles/bingit-prompt.sh:
GIT_PS1_SHOWDIRTYSTATE=1

#PS1='[\u@\h \W]\$ ' #old prompt config
# evaluate how to tweak this:
#PS1='\[\033[0;32m\]\[\033[0m\033[1m\]\u\[\033[1;34m\]@\[\033[1;34m\]\h \w\[\033[0;31m\]\]$(__git_ps1)\n\[\033[0;32m\]└─\[\033[0m\033[0m\] \$\[\033[0;31m\]$(__awsenv_ps1 2>/dev/null)\[\033[0m\033[0m\]:\[\033[0m\] '
#PS1='\[\033[0;32m\]\[\033[0m\033[1m\]\u\[\033[1;34m\]@\[\033[1;34m\]\h \w \[\033[0;41m\]\]$(__git_ps1 "%s")\[\033[0;31m\]\]\n\[\033[0;32m\]└─\[\033[0m\033[0m\] \[\033[0;31m\](\[\033[0;41m\]$AWS_PROFILE\[\033[0;31m\])\[\033[0m\033[0m\]\$:\[\033[0m\] '
PS1='\[\033[0;32m\]\[\033[0m\033[1m\]\u\[\033[1;34m\]@\[\033[1;34m\]\h \w \[\033[0;41m\]\]$(__git_ps1 "%s")\[\033[0;31m\]\]\n\[\033[0;32m\]└─\[\033[0m\033[0m\] \[\033[0;31m\](\[\033[0;41m\]$(__cloudprovider_context)\[\033[0;31m\])\[\033[0m\033[0m\]\$:\[\033[0m\] '
#export PS1

#----------------------[CLOUD PROVIDER BEGIN]-----------------------------------------
export CLOUDPROVIDER="${CLOUDPROVIDER:-NONE}"
# DONE set target cloud provider via ENV variable
# DONEprovide an alias for conveniently selecting/switching between cloud providers
#   TODO add autocompletion
# TODO provide a generic function for providing contextual information for the selected cloud provider
#   TODO if CLOUDPROVIDER is set to 'NONE' no information shall be given
# TODO provide convenience aliases/functions for switching between 'projects'/'roles'/'subscriptions' per cloud provider (aws, gcloud, azure)
alias select-cloudprovider="__switch_cloudprovider"
alias switch-cloudprovider="__switch_cloudprovider"
alias set-cloudprovider="__switch_cloudprovider"

__switch_cloudprovider() {
    CLOUDPROVIDER=$1
    #COMPLETION="AWS AZURE GCP NONE"

    #VERIFY that the required command(s) are available
    #TODO use a map for making the code DRY
    if [[ ${CLOUDPROVIDER} ==  "GCP" ]];
    then
        if ! command -v gcloud &> /dev/null
        then
            echo "gcloud is not installed! (google-cloud-sdk). RESETTING env var CLOUDPROVIDER to 'NONE'"
            CLOUDPROVIDER="NONE"
            return 1
        fi
    elif [[ ${CLOUDPROVIDER} ==  "AWS" ]];
    then
        if ! command -v aws &> /dev/null
        then
            echo "aws is not installed! (awscli). RESETTING env var CLOUDPROVIDER to 'NONE'"
            CLOUDPROVIDER="NONE"
            return 1
        fi
    elif [[ ${CLOUDPROVIDER} == "AZURE" ]];
    then
        if ! command -v azure &> /dev/null
        then
            echo "azure is not installed! (aur/azure-cli). RESETTING env var CLOUDPROVIDER to 'NONE'"
            CLOUDPROVIDER="NONE"
            return 1
        fi

    fi
}

__cloudprovider_context() {
    if [[ ${CLOUDPROVIDER} =~ (AWS|aws) ]]; then
        printf "$AWS_PROFILE"
    elif [[ ${CLOUDPROVIDER} =~ (AZURE|azure|AZ|az) ]]; then
        printf "TBD"
    elif [[ ${CLOUDPROVIDER} =~ (GCP|gcp|GOOGLE|google) ]]; then
        read -d "\n" CONFIG_NAME PROJECT_NAME <<< $(gcloud config configurations list | awk '{ if ($2 == "True") { print $1 "\n" $4 }}')
        printf "${CONFIG_NAME} > ${PROJECT_NAME}"
    elif [[ ${CLOUDPROVIDER} == "NONE" ]]; then
        printf ""
    else
        printf "UNDEFINED"
    fi
    return 0
}

#TODO merge google_project_completion and gswitch_completion into one command - if possible
__google_project_completion() {
    COMPREPLY=()
    local currentWord=${COMP_WORDS[COMP_CWORD]}
    local projects=$(gcloud projects list | awk '{if ($1 != "PROJECT_ID") print $1}')
    COMPREPLY=($(compgen -W "$projects" -- "$currentWord"))
    return 0
}

__gswitch_completion() {
    COMPREPLY=()
    local currentWord=${COMP_WORDS[COMP_CWORD]}
    local configurations=$(ls -l ${HOME}/.config/gcloud/configurations | grep -Po "(config_)\K.*" --color=never)
    COMPREPLY=($(compgen -W "$configurations" -- "$currentWord"))
    return 0
}
#----------------------[CLOUD PROVIDER END]-----------------------------------------


#TERM='rxvt-unicode'
#try to  set term for tmux:
#TERM='xterm-256color'
COLORTERM='rxvt-unicode-256color'


# find running ssh-agent and use it
# info from: http://blog.joncairns.com/2013/12/understanding-ssh-agent-and-ssh-add/
#source ~/dotfiles/bin/ssh-find-agent
source ~/dotfiles/bin/ssh-find-agent -a || eval $(ssh-agent) > /dev/null    # automatically start an ssh-agent if none is found
set_ssh_agent_socket


#TODO is this for osx?
source ~/dotfiles/bin/ls-colors

##autocomplete ssh
#Thanks to: https://gist.github.com/magnetikonline/bcd4186e14ed02145390
function _completeSSHHosts {

    COMPREPLY=()
    local currentWord=${COMP_WORDS[COMP_CWORD]}

    local completeHosts=$(
        cat "$HOME/.ssh/config" | \
        grep --extended-regexp --regexp "^(Host|host) +[^* ]+? *$" | \
        tr -s " " | cut -d " " -f 2;
        cat /etc/hosts | \
        grep --extended-regexp --regexp "^[0-9]{3}\." | \
        awk "{print \$2}"
    )

    COMPREPLY=($(compgen -W "$completeHosts" -- "$currentWord"))
    return 0
}

#FIXME this is not working yet
alias ansible_vim="vim"
function _complete_ansible_modules {
    COMPREPLY=()
    local currentWord="${COMP_WORDS[COMP_CWORD]}"
    local previousWord="${COMP_WORDS[COMP_CWORD-1]}"

    local ansible_python_module_location=$(ansible --version | grep -Po "ansible python module location = \K.*")

    local completeAnsibleModule=$(ls -1QD --color=never "${ansible_python_module_location}")
    COMPREPLY=($(compgen -W "${completeAnsibleModule}" -- "${currentWord}"))
    return 0
}
complete -o nospace -F _complete_ansible_modules -o filenames default ansible_vim
#complete -F _complete_ansible_modules -o filenames default ansible_vim

#[ -f ~/.fzf.bash ] && source ~/.fzf.bash
export FZF_DEFAULT_OPTS='
    --color fg:252,bg:#2a2a2a,hl:67,fg+:252,bg+:235,hl+:81
    --color info:144,prompt:161,spinner:135,pointer:135,marker:118'
#see https://sidneyliebrand.io/blog/how-fzf-and-ripgrep-improved-my-workflow
export FZF_DEFAULT_COMMAND='rg --files --no-ignore-vcs --hidden'

[ -f ~/dotfiles/aws-get-session-token.sh ] && source ~/dotfiles/aws-get-session-token.sh #TODO rename to aws-functions.sh
[ -f /usr/share/fzf/completion.bash ] && source /usr/share/fzf/completion.bash
[ -f /usr/share/fzf/key-bindings.bash ] && source /usr/share/fzf/key-bindings.bash
[ -d /usr/share/bash-completion/completions/ ] && source /usr/share/bash-completion/completions/*
#[ -f /usr/share/bash-completion/completions/git ] && source /usr/share/bash-completion/completions/git
#[ -f /usr/share/bash-completion/completions/docker ] && source /usr/share/bash-completion/completions/docker
#export CLOUDSDK_PYTHON='/usr/bin/python2.7'
alias gswitch="gcloud config configurations activate"
alias gswitch-project="gcloud config set project"
[ -f /etc/bash_completion.d/google-cloud-sdk ] && source /etc/bash_completion.d/google-cloud-sdk
complete -F __gswitch_completion gswitch
complete -F __google_project_completion gswitch-project

#this is a dirty workaround to get tab completion for glow
#see issue: https://github.com/charmbracelet/glow/issues/457
complete -o default -o bashdefault glow
#this is a dirty workaround to provide both: autocompletion via fzf and custom autocompletion for ssh
complete -F _fzf_complete_ssh -o default -o bashdefault -F _completeSSHHosts ssh
complete -F _completeSSHHosts -o default -b bashdefault scp
#complete -C /usr/bin/aws_completer aws #use command completion for bash
complete -C ${HOME}/.local/bin/aws_completer aws
