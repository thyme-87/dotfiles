#
# ~/.bashrc
# This file is executed for interactive non-login shells.
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

#-------------- ALIAS BEGIN----------------
alias tor='~/tor-browser_en-US/start-tor-browser --detach'
alias chromium='setsid /usr/bin/chromium >& /dev/null &'
alias bopdf='~/Dropbox/BerlinOnline/4_other/bo_pdf/bopdf.sh'

# make ls use colors automatically
alias ls='ls --color=auto'
#-------------- ALIAS END -----------------

PS1='[\u@\h \W]\$ '

export EDITOR="vim" 

PATH=$PATH:~/dotfiles/bin

export PATH

#TERM='rxvt-unicode'
#try to  set term for tmux:
#TERM='screen-256color'
#TERM='rxvt-unicode--256color'
COLORTERM='rxvt-unicode-256color'


# find running ssh-agent and use it
# info from: http://blog.joncairns.com/2013/12/understanding-ssh-agent-and-ssh-add/
#source ~/dotfiles/bin/ssh-find-agent
#set_ssh_agent_socket
