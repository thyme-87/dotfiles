#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

#set editor to VIM
export VISUAL=vim
export EDITOR="$VISUAL"

#add dotfiles/bin to PATH
#NOTE that this will not affect root!

# currently I'm not using cabal for some reasons (cabal-hell)
PATH=$PATH:~/dotfiles/bin
#PATH=$PATH:~/.cabal/bin

xrdb  ~/.Xresources

# make sure that .bashrc is also executed
source  ~/.bashrc

export PATH
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
