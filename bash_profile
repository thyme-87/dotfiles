#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

#set editor to VIM
export VISUAL=vim
export EDITOR="$VISUAL"

#add dotfiles/bin to PATH
#NOTE that this will not affect root!

PATH=$PATH:~/dotfiles/bin
export PATH
