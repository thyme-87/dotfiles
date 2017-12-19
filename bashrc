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
alias ruby-local='PATH=./bin:$PATH'

# make ls use colors automatically
alias ls='ls -G'
#-------------- ALIAS END -----------------


source ~/dotfiles/bin/git-prompt.sh

#PS1='[\u@\h \W]\$ ' #old prompt config
# evaluate how to tweak this:
PS1='\[\033[0;32m\]\[\033[0m\033[1m\]\u\[\033[1;34m\]@\[\033[1;34m\]\h \w\[\033[0;31m\]$(__git_ps1)\n\[\033[0;32m\]└─\[\033[0m\033[0m\] \$:\[\033[0m\] '

export EDITOR="vim" 
export JAVA_HOME=/usr/bin/java
export LC_ALL=en_US.UTF-8

PATH=/usr/local/bin:$PATH
PATH=$PATH:~/dotfiles/bin
PATH=$PATH:~/bin #local bin for personal shell scripts

export PATH

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

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
