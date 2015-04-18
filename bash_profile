#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

#add dotfiles/bin to PATH
#NOTE that this will not affect root!

PATH=$PATH:~/dotfiles/bin
export PATH
