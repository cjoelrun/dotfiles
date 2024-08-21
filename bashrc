#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls -lhG'
PS1='[\u@\h \W]\$ '

export PATH=~/bin:~/.cabal/bin:~/.xmonad/bin:$PATH

alias watch_razor="watch -n 5 -d 'ssh cameron@razor \"razor active_model | wc -l ; razor active_model ; razor node | wc -l ; razor node\"'"
alias watch_chef="watch -n 5 -d 'ssh cameron@razor \"knife node list | wc -l ; knife node list\"'"
alias knife-personal='rm .chef; ln -s ~/.chefs/personal ~/.chef'
alias knife-qa='rm .chef; ln -s ~/.chefs/qa ~/.chef'
alias nodes='knife node list'
alias envs='knife environment list'
alias clients='knife client list'
alias upgrade_opencafe='for i in "opencafe" "cloudcafe" "cloudroast"; do echo "cd ~/work/cloudcafe/${i}; git fetch --all; sudo pip install -r pip-requires; sudo pip install . --upgrade; cd -" | bash -x; done'
alias "knifels"='ls ~/.chefs'
rknife() {
    knife search node "role:*$1*"
}
nknife() {
    knife search node "name:*$1*"
}
eknife() {
    knife search node "chef_environment:*$1*"
}
uknife() {
    knife search node "in_use:*$1*"
}
knifeset() {
    rm -f ~/.chef
    ln -s ~/.chefs/$1 ~/.chef
}
dracip() {
    knife node show $1 -a network_interfaces.drac | awk '{if ($2) print $2;}'
}
sshn() {
    sshpass -p `knife node show $1 -a password | grep password | awk '{print $2}'` ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -o LogLevel=quiet -l root `knife node show $1 -a ipaddress | grep ipaddress| awk '{print $2}'`
}

export BROWSER=google-chrome
export EDITOR='emacs -nw'

# source `which virtualenvwrapper.sh`
WORKON_HOME=$HOME/.venvs

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"
PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

# homebrew
export PATH="/usr/local/sbin:$PATH"
export PATH="/usr/local/bin:$PATH"

export PATH="$PATH:/Applications/Postgres.app/Contents/MacOS/bin"

# tex
export PATH="$PATH:/Library/TeX/texbin"

export CFLAGS=-Qunused-arguments
export CPPFLAGS=-Qunused-arguments
export ARCHFLAGS=-Wno-error=unused-command-line-argument-hard-error-in-future

#THIS MUST BE AT THE END OF THE FILE FOR GVM TO WORK!!!
[[ -s "/Users/cameron/.gvm/bin/gvm-init.sh" ]] && source "/Users/cameron/.gvm/bin/gvm-init.sh"

alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs -nw'

export JAVA_HOME=$(/usr/libexec/java_home)

export PROJECT_HOME=$HOME/.projects
# source $HOME/.dvm/dvm.sh
# source $HOME/.secrets

export REACT_EDITOR="emacs -q"

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/Users/came5758/.sdkman"
[[ -s "/Users/came5758/.sdkman/bin/sdkman-init.sh" ]] && source "/Users/came5758/.sdkman/bin/sdkman-init.sh"

export NVM_DIR="/Users/came5758/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/home/cameron/.sdkman"
[[ -s "/home/cameron/.sdkman/bin/sdkman-init.sh" ]] && source "/home/cameron/.sdkman/bin/sdkman-init.sh"

if [[ -f $TMPDIR/ginger-cli.sh ]]; then
    source $TMPDIR/ginger-cli.sh
fi
. "$HOME/.cargo/env"
