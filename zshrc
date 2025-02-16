export PROJECT_HOME=$HOME/work
export JSON_DICTIONARY_REPO=$HOME/.configstore

export EDITOR="code -w"
export REACT_EDITOR='/usr/local/bin/code'

export ANDROID_HOME=${HOME}/Library/Android/sdk
export PATH=${PATH}:${ANDROID_HOME}/tools
export PATH=${PATH}:${ANDROID_HOME}/platform-tools

export SDKMAN_DIR="~/.sdkman"
[[ -s "~/.sdkman/bin/sdkman-init.sh" ]] && source "~/.sdkman/bin/sdkman-init.sh"

# tabtab source for serverless package
# uninstall by removing these lines or running `tabtab uninstall serverless`
[[ -f /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.zsh ]] && . /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.zsh
# tabtab source for sls package
# uninstall by removing these lines or running `tabtab uninstall sls`
[[ -f /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.zsh ]] && . /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.zsh

export PATH="/opt/homebrew/opt/ruby/bin:/opt/homebrew/lib/ruby/gems/3.1.0/bin/:$PATH"
export PATH="$HOME/.yarn/bin:$PATH"
export LDFLAGS="-L/opt/homebrew/opt/ruby/lib"
export CPPFLAGS="-I/opt/homebrew/opt/ruby/include"

NPM_TOKEN_LINE=$(grep -E '^//registry.npmjs.org/:_authToken=' $HOME/.npmrc)

if [ -n "$NPM_TOKEN_LINE" ]; then
    # Extract the token from the line
    NPM_TOKEN=$(echo $NPM_TOKEN_LINE | sed 's|^//registry.npmjs.org/:_authToken=||')

    # Export the token as an environment variable
    export NPM_TOKEN
else
    echo "No npm token found in .npmrc file."
fi

# This is an example, fill in your own Role here
export TERRAFORM_EXEC_ROLE=TerraformState
export TF_VAR_terraform_exec_role=${TERRAFORM_EXEC_ROLE}
export PATH="$(go env GOPATH)/bin:$PATH"

export PATH="$HOME/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"

cd_gi() {
  cd $CODE_PATH$1
}

source $HOME/.secrets
# Created by `pipx` on 2024-11-21 04:12:52
export PATH="$PATH:/Users/cameronlopez/.local/bin"
