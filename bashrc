#
# ~/.bashrc
# This file is executed for interactive non-login shells.
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

#-------------- ALIAS BEGIN----------------
alias tor='~/tor-browser_en-US/start-tor-browser --detach'
alias bopdf='~/Dropbox/BerlinOnline/4_other/bo_pdf/bopdf.sh'
alias mychromium='setsid /usr/bin/chromium >& /dev/null &'
alias lookup="fzf --preview 'highlight -O ansi -l {}'"
alias vimup="vim \$(lookup)"
alias aws_list_instance_names="aws ec2 describe-instances | jq '.[][].Instances[].Tags[] | {Instance_Name: select(.Key==\"Name\").Value}'"


function __set_aws_profile {
    export AWS_PROFILE=$1
}

function __complete_aws_profile {
	local currentWord=${COMP_WORDS[COMP_CWORD]}
    AWS_PROFILES_COMPLETION_LIST=$(cat "$HOME/.aws/config" | \
        grep --extended-regexp "\[(profile|Profile)" | \
        sed -E "s/^\[(Profile|profile)\s+(.*)\].?*/\2/g")
    COMPREPLY=($(compgen -W "$AWS_PROFILES_COMPLETION_LIST" "$currentWord"))
    return 0
}

alias awsprofile='__set_aws_profile'
complete -F __complete_aws_profile awsprofile

# make ls use colors automatically
alias ls='ls -G'
#-------------- ALIAS END -----------------


source ~/dotfiles/bin/git-prompt.sh


export EDITOR="vim" 
export JAVA_HOME=/usr/bin/java
export LC_ALL=en_US.UTF-8
export GPG_TTY=`tty` #to make gpg work on osx; see https://github.com/deepmind/kapitan/issues/69

PATH=/usr/local/bin:$PATH
PATH=$PATH:~/dotfiles/bin
PATH=$PATH:~/bin #local bin for personal shell scripts
PATH=$PATH:~/Library/Python/3.6/bin/ #for software installed via pip (aws cli)

export PATH

#PS1='[\u@\h \W]\$ ' #old prompt config
# evaluate how to tweak this:
#PS1='\[\033[0;32m\]\[\033[0m\033[1m\]\u\[\033[1;34m\]@\[\033[1;34m\]\h \w\[\033[0;31m\]$(__git_ps1)\n\[\033[0;32m\]└─\[\033[0m\033[0m\] \$\[\033[0;31m\]$(__awsenv_ps1 2>/dev/null)\[\033[0m\033[0m\]:\[\033[0m\] '
PS1='\[\033[0;32m\]\[\033[0m\033[1m\]\u\[\033[1;34m\]@\[\033[1;34m\]\h \w\[\033[1;31m\]$(__git_ps1)\n\[\033[0;32m\]└─\[\033[0m\033[0m\] \[\033[0;31m\](\[\033[0;45m\]$AWS_PROFILE\[\033[0;31m\])\[\033[0m\033[0m\]\$:\[\033[0m\] '
#export PS1

#TERM='rxvt-unicode'
#try to  set term for tmux:
TERM='xterm-256color'
COLORTERM='xrv-unicode-256color'


# find running ssh-agent and use it
# info from: http://blog.joncairns.com/2013/12/understanding-ssh-agent-and-ssh-add/
#source ~/dotfiles/bin/ssh-find-agent
source ~/dotfiles/bin/ssh-find-agent -a || eval $(ssh-agent) > /dev/null    # automatically start an ssh-agent if none is found
set_ssh_agent_socket

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

[ -f ~/dotfiles/aws-get-session-token.sh ] && source ~/dotfiles/aws-get-session-token.sh

#this is a dirty workaround to provide both: autocompletion via fzf and custom autocompletion for ssh
complete -F _fzf_complete_ssh -o default -o bashdefault -F _completeSSHHosts ssh
complete -F _completeSSHHosts -o default -b bashdefault scp
complete -C aws_completer aws #use command completion for bash
