#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '
export EDITOR="vim" 

# search for running ssh-agent and use it

ssh-find-agent -a
if [ -z "$SSH_AUTH_SOCK" ]
then
	   eval $(ssh_agent) > /dev/null
	      ssh-add -l >/dev/null || alias ssh='ssh-add -l >/dev/null || ssh-add && unalias ssh; ssh'
      fi
