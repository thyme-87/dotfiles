#
# ~/.bash_profile
# This file is executed for login shells
#

[[ -f ~/.bashrc ]] && . ~/.bashrc
#set editor to VIM
export VISUAL=vim
export EDITOR="$VISUAL"

#use vi keybindings for bash
#set -o vi

#add dotfiles/bin to PATH
#NOTE that this will not affect root!

PATH=/Library/Tex/texbin:$PATH
PATH=/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/X11/bin:$PATH
PATH=/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/X11/bin:$PATH
PATH=$PATH:~/dotfiles/bin
export PATH

#########################
# START CUSTOM ENV VARS #
#########################
export TIME_TRACKING_LOGFILE=~/.task.log
#########################
#  END CUSTOM ENV VARS  #
#########################

# make sure that .bashrc is also executed
source  ~/.bashrc

#export PATH
#look for existing ssh-agent and use it, otherwise start new ssh-agent
#script from: https://stackoverflow.com/questions/18880024/start-ssh-agent-on-login
#SSH_ENV="$HOME/.ssh/environment"
#
#function start_agent {
#    echo "Initialising new SSH agent..."
#    /usr/bin/ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
#    echo succeeded
#    chmod 600 "${SSH_ENV}"
#    . "${SSH_ENV}" > /dev/null
#    /usr/bin/ssh-add;
#}

# Source SSH settings, if applicable
#
#if [ -f "${SSH_ENV}" ]; then
#	. "${SSH_ENV}" > /dev/null
#	 #ps ${SSH_AGENT_PID} doesn't work under cywgin
#	ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
#		start_agent;
#	}
#        else
#                start_agent;
#fi

if which rbenv > /dev/null;
    then eval "$(rbenv init -)";
        echo "fuck os x!";
fi

##
# Your previous /Users/tschroeder/.bash_profile file was backed up as /Users/tschroeder/.bash_profile.macports-saved_2017-12-13_at_17:18:56
##

#Set path + manpath for coreutils
#PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH" #we use coreutils fore reasonable sed etc.
PATH="/usr/local/sbin:$PATH"
PATH="/usr/local/opt/openssl/bin:$PATH" #for Openssl
# Finished adapting your PATH environment variable for use with MacPorts.
PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"

MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"
MANPATH="/usr/local/Cellar:$MANPATH"
export MANPATH

#Add pip3 install folder to PATH
#TODO get Python 3.6 and Python 2.7 sorted out correctly
#PATH=~/Library/Python/3.6/bin/:$PATH #for software installed via pip (aws cli)

#use php@7.1 instead of php56
export PATH="/usr/local/opt/php@7.1/bin:$PATH"
export PATH="/usr/local/opt/php@7.1/sbin:$PATH"
# MacPorts Installer addition on 2017-12-13_at_17:18:56: adding an appropriate PATH variable for use with MacPorts.
# evaluate jenv so that we can use java
eval "$(jenv init -)"
