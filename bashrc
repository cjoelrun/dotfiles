#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls -lhG'
PS1='[\u@\h \W]\$ '

export PATH=~/bin:~/.cabal/bin:~/.xmonad/bin:$PATH

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

# Alias for Claude CLI with named conversations
alias claude='claude-named'

export JAVA_HOME=$(/usr/libexec/java_home)

export PROJECT_HOME=$HOME/.projects
# source $HOME/.dvm/dvm.sh
source $HOME/.secrets

export REACT_EDITOR="emacs -q"

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/Users/came5758/.sdkman"
[[ -s "/Users/came5758/.sdkman/bin/sdkman-init.sh" ]] && source "/Users/came5758/.sdkman/bin/sdkman-init.sh"

export NVM_DIR="/Users/came5758/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/home/cameron/.sdkman"
[[ -s "/home/cameron/.sdkman/bin/sdkman-init.sh" ]] && source "/home/cameron/.sdkman/bin/sdkman-init.sh"

. "$HOME/.cargo/env"

# Added by LM Studio CLI (lms)
export PATH="$PATH:/Users/cameronlopez/.lmstudio/bin"
