#!/bin/zsh
if [[ -x /opt/homebrew/bin/brew ]] ; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
    FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"
    autoload -Uz compinit
    compinit
fi
alias reload='source ~/.zshrc'
export DOTFILES_ROOT=$(readlink $HOME/dotfiles)
export PATH="~/.local/bin:$PATH"

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
source "${DOTFILES_ROOT}/bin/common.sh"
source "${DOTFILES_ROOT}/bin/zload.zsh" && zload

export EMACSD="$HOME/.emacs.d"
export CMACSD="$HOME/.cmacs.d"
export TMPD="$HOME/tmp"

export GO_VERSION=1.24.3
export GOENV_ROOT="$HOME/.goenv"
export GOPATH="$HOME/go"

export PY_VERSION=3.13.3
export PYENV_ROOT="$HOME/.pyenv"

export RB_VERSION=3.2.2
export RBENV_ROOT="$HOME/.rbenv"

export NODE_VERSION=v23.11.1
export NVM_DIR="$HOME/.nvm"
export NPM_ROOT="$HOME/.npm"

export CARGO_HOME="$HOME/.cargo"

# go
eval "$(goenv init -)"
export PATH="$PATH:$GOENV_ROOT/bin"
export PATH="$PATH:$GOPATH/bin"
export PATH="$PATH:$GOROOT/bin"
load_go() {
    goenv shell "$GO_VERSION"
    go version
}
# python
export PATH="$PYENV_ROOT/bin:$PYENV_ROOT/pyenv/bin:$PATH"
export PYTHONSTARTUP="$HOME/.pythonrc.py"
eval "$(pyenv init -)"
load_python() {
    pyenv shell "$PY_VERSION"
    python -V
}
# ruby
export PATH="$RBENV_ROOT/bin:$PATH"
export PATH="$(gem environment gemdir)/bin:$PATH"
eval "$(rbenv init - zsh)"
load_ruby() {
    rbenv shell "$RB_VERSION"
    ruby -v
}
# rust
export PATH="$CARGO_HOME/bin:$PATH"
# node
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

export PATH="$PATH:$(go env GOPATH)/bin"
# zplugins
## pure
PURE_CMD_MAX_EXEC_TIME=10
zstyle ':prompt:pure:prompt:success' color green
zstyle ':prompt:pure:prompt:error' color red
zstyle ':prompt:pure:path' color white
## autosuggestions
bindkey '^ ' autosuggest-accept
