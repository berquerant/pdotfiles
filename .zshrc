#!/bin/zsh

[[ -x /opt/homebrew/bin/brew ]] && eval "$(/opt/homebrew/bin/brew shellenv)"
alias reload='source ~/.zshrc'
export DOTFILES_ROOT=$(readlink $HOME/dotfiles)
source $DOTFILES_ROOT/bin/init.zsh
export PATH="~/.local/bin:$PATH"
eval "$(direnv hook zsh)"

export ZPLUG_HOME=$(brew --prefix zplug)
source $ZPLUG_HOME/init.zsh
zplug 'zplug/zplug', hook-build:'zplug --self-manage'
zplug "mafredri/zsh-async"
zplug "sindresorhus/pure"
zplug "b4b4r07/enhancd", use:init.sh
zplug "zsh-users/zsh-autosuggestions"
zplug "zsh-users/zsh-history-substring-search", defer:2
zplug "zsh-users/zsh-completions"
if ! zplug check ; then zplug install ; fi
zplug load

source $DOTFILES_ROOT/bin/forward.zsh
source $DOTFILES_ROOT/bin/time.zsh
source $DOTFILES_ROOT/bin/tmux.zsh
source $DOTFILES_ROOT/bin/emacs.zsh
source $DOTFILES_ROOT/bin/git.zsh
source $DOTFILES_ROOT/bin/peco.zsh
source $DOTFILES_ROOT/bin/util.zsh

# go
export GOPATH="$HOME/go"
export PATH="$PATH:$GOPATH/bin"
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
export PATH="$(brew --prefix ruby)/bin:$PATH"
export PATH="$(gem environment gemdir)/bin:$PATH"
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
# lisp
export ROS_ROOT="$HOME/.roswell"
export CL_COMPILER=sbcl
export CL_COMPILER_VERSION=2.0.7
export PATH="$ROS_ROOT/bin:$PATH"
# haskell
export GHCUP_ROOT="$HOME/.ghcup"
export PATH="$GHCUP_ROOT/bin:$PATH"
# flutter
export FLUTTER_ROOT="$HOME/.flutter"
export PATH="$FLUTTER_ROOT/flutter/bin:$PATH"
# activate lazy envs
load_envs() {
    pyenv --version
    nvm --version
}

# zplugins
## pure
PURE_CMD_MAX_EXEC_TIME=10
zstyle ':prompt:pure:prompt:success' color green
zstyle ':prompt:pure:prompt:error' color red
zstyle ':prompt:pure:path' color white
## enhancd
export ENHANCD_FILTER=peco
## autosuggestions
bindkey '^ ' autosuggest-accept
