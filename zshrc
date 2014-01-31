# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=100000
setopt autocd extendedglob
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/cameron/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

PATH=$PATH:$HOME/bin # Add home bin to PATH
PATH=$PATH:$HOME/.cabal/bin # Add cabal to PATH
PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
PATH=/usr/local/heroku/bin:$PATH # Add heroku to path

WORKON_HOME=$HOME/.venvs
source /usr/bin/virtualenvwrapper.sh

[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"

EDITOR="emacsclient -nw"
BROWSER="conkeror"

alias ls='ls -lhG'
alias watch_razor="watch -n 5 -d 'ssh cameron@razor-chef \"razor active_model | wc -l ; razor active_model ; razor node | wc -l ; razor node\"'"
alias watch_chef="watch -n 5 -d 'ssh cameron@razor-chef \"knife node list | wc -l ; knife node list\"'"
alias knife-personal='rm -rf .chef; ln -s ~/.chefs/personal ~/.chef'
alias knife-qa='rm -rf .chef; ln -s ~/.chefs/qa ~/.chef'
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
