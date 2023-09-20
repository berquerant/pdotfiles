#!/bin/zsh

[[ -x /opt/homebrew/bin/brew ]] && eval "$(/opt/homebrew/bin/brew shellenv)"
alias reload='source ~/.zshrc'
export DOTFILES_ROOT=$(readlink $HOME/dotfiles)
export PATH="~/.local/bin:$PATH"
eval "$(direnv hook zsh)"

export ZPLUG_HOME=$(brew --prefix zplug)
source $ZPLUG_HOME/init.zsh
zplug 'zplug/zplug', hook-build:'zplug --self-manage'
zplug "mafredri/zsh-async"
zplug "sindresorhus/pure"
zplug "zsh-users/zsh-autosuggestions"
zplug "zsh-users/zsh-history-substring-search", defer:2
zplug "zsh-users/zsh-completions"
if ! zplug check ; then zplug install ; fi
zplug load
source "${DOTFILES_ROOT}/bin/zload.zsh" && zload

# go
export GOENV_ROOT="$HOME/.goenv"
export PATH="$GOENV_ROOT/bin:$PATH"
export GO_VERSION="1.20.5"
export GOPATH="$HOME/go"
export PATH="$PATH:$GOPATH/bin"
export PATH="$GOROOT/bin:$PATH"
eval "$(goenv init -)"
load_go() {
    goenv shell "$GO_VERSION"
    go version
}
# python
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PYENV_ROOT/pyenv/bin:$PATH"
export PY_VERSION="3.11.4"
export PYTHONSTARTUP="$HOME/.pythonrc.py"
export PYTHONHISTORY="$HOME/.python.history"
eval "$(pyenv init -)"
load_python() {
    pyenv shell "$PY_VERSION"
    python -V
}
# ruby
export RBENV_ROOT="$HOME/.rbenv"
export RB_VERSION="3.2.2"
export PATH="$RBENV_ROOT/bin:$PATH"
export PATH="$(gem environment gemdir)/bin:$PATH"
eval "$(rbenv init - zsh)"
load_ruby() {
    rbenv shell "$RB_VERSION"
    ruby -v
}
# rust
export CARGO_HOME="$HOME/.cargo"
export PATH="$CARGO_HOME/bin:$PATH"
# node
export NVM_DIR="$HOME/.nvm"
export NPM_ROOT="$HOME/.npm"
export NODE_VERSION="v20.6.1"
source "$NVM_DIR/nvm.sh"
load_node() {
    nvm use "$NODE_VERSION"
    node --version
}

load_env() {
    load_go
    load_python
    load_ruby
    load_node
}
load_env
# zplugins
## pure
PURE_CMD_MAX_EXEC_TIME=10
zstyle ':prompt:pure:prompt:success' color green
zstyle ':prompt:pure:prompt:error' color red
zstyle ':prompt:pure:path' color white
## autosuggestions
bindkey '^ ' autosuggest-accept
