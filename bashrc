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
alias weather='weather-report eddb'
alias csv-view='__csv_view'
alias b64decode='__base64_decode'
alias ll='ls -l'
alias gnome-settings='XDG_CURRENT_DESKTOP=GNOME gnome-control-center'
alias gitp='git push'

#alias ansdoc='ansible-doc $(rg -g "!test/" -g "!tests/" -g "!module_utils/" -g "!doc_fragments/" -g "!cliconf/" -g "!terminal/" -g "!action/" -g "!httpapi/" -g "!callback/" --iglob "*.py" --iglob "!__init__.py" --files /usr/lib/python3.8/site-packages/ansible_collections/ /usr/lib/python3.8/site-packages/ansible/modules/ | grep -oP "(ansible_collections/|ansible/modules/)\K.*" | sed -e 's!plugins/modules/!!' -e 's!/!.!g' -e "s!\.py!!" | fzf -e)'
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

#convenience for fzf
alias lookup="fzf --preview 'highlight -O ansi -l {}'"
alias vimup="vim \$(lookup)"

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

#indication via ~/dotfiles/bingit-prompt.sh:
GIT_PS1_SHOWDIRTYSTATE=1

#PS1='[\u@\h \W]\$ ' #old prompt config
# evaluate how to tweak this:
#PS1='\[\033[0;32m\]\[\033[0m\033[1m\]\u\[\033[1;34m\]@\[\033[1;34m\]\h \w\[\033[0;31m\]\]$(__git_ps1)\n\[\033[0;32m\]└─\[\033[0m\033[0m\] \$\[\033[0;31m\]$(__awsenv_ps1 2>/dev/null)\[\033[0m\033[0m\]:\[\033[0m\] '
PS1='\[\033[0;32m\]\[\033[0m\033[1m\]\u\[\033[1;34m\]@\[\033[1;34m\]\h \w \[\033[0;41m\]\]$(__git_ps1 "%s")\[\033[0;31m\]\]\n\[\033[0;32m\]└─\[\033[0m\033[0m\] \[\033[0;31m\](\[\033[0;41m\]$AWS_PROFILE\[\033[0;31m\])\[\033[0m\033[0m\]\$:\[\033[0m\] '
#export PS1

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

#[ -f ~/.fzf.bash ] && source ~/.fzf.bash
export FZF_DEFAULT_OPTS='
    --color fg:252,bg:#2a2a2a,hl:67,fg+:252,bg+:235,hl+:81
    --color info:144,prompt:161,spinner:135,pointer:135,marker:118
'
[ -f ~/dotfiles/aws-get-session-token.sh ] && source ~/dotfiles/aws-get-session-token.sh #TODO rename to aws-functions.sh
[ -f /usr/share/fzf/completion.bash ] && source /usr/share/fzf/completion.bash
[ -f /usr/share/fzf/key-bindings.bash ] && source /usr/share/fzf/key-bindings.bash
[ -d /usr/share/bash-completion/completions/ ] && source /usr/share/bash-completion/completions/*
#[ -f /usr/share/bash-completion/completions/git ] && source /usr/share/bash-completion/completions/git
#[ -f /usr/share/bash-completion/completions/docker ] && source /usr/share/bash-completion/completions/docker


#this is a dirty workaround to provide both: autocompletion via fzf and custom autocompletion for ssh
complete -F _fzf_complete_ssh -o default -o bashdefault -F _completeSSHHosts ssh
complete -F _completeSSHHosts -o default -b bashdefault scp
complete -C /usr/bin/aws_completer aws #use command completion for bash
