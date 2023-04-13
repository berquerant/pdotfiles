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

find "${DOTFILES_ROOT}/bin/z" -type f | sort | while read x ; do source "$x" ; done

# go
export GOENV_ROOT="$HOME/.goenv"
export PATH="$GOENV_ROOT/bin:$PATH"
export GOPATH="$HOME/go"
export PATH="$PATH:$GOPATH/bin"
export PATH="$GOROOT/bin:$PATH"
goenv() { # lazy
    unset -f goenv
    source <(goenv init -)
    goenv "$@"
}
# python
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
export PY_VERSION=3.10.0
export PYTHONSTARTUP="$HOME/.pythonrc.py"
export PYTHONHISTORY="$HOME/.python.history"
pyenv() { # lazy
    unset -f pyenv
    source <(pyenv init --path)
    source <(pyenv init -)
    pyenv "$@"
}
# ruby
export RBENV_ROOT="$HOME/.rbenv"
export RB_VERSION=3.2.2
export PATH="$(gem environment gemdir)/bin:$PATH"
rbenv() { # lazy
    unset -f rbenv
    eval "$(~/.rbenv/bin/rbenv init - zsh)"
    rbenv "$@"
}
# rust
export CARGO_HOME="$HOME/.cargo"
export PATH="$CARGO_HOME/bin:$PATH"
# node
export NVM_DIR="$HOME/.nvm"
export NPM_ROOT="$HOME/.npm"
nvm() { # lazy
    unset -f nvm
    source "$NVM_DIR/nvm.sh"
    nvm "$@"
}
# activate lazy envs
load_envs() {
    goenv --version
    goenv global "$GO_VERSION"
    go version
    pyenv --version
    pyenv global "$PY_VERSION"
    python -V
    echo "nvm $(nvm --version)"
    node --version
    rbenv --version
    rbenv global "$RB_VERSION"
    ruby --version
}

# zplugins
## pure
PURE_CMD_MAX_EXEC_TIME=10
zstyle ':prompt:pure:prompt:success' color green
zstyle ':prompt:pure:prompt:error' color red
zstyle ':prompt:pure:path' color white
## autosuggestions
bindkey '^ ' autosuggest-accept
