#!/bin/zsh

zload() {
    source "${DOTFILES_ROOT}/.env"
    export EMACSD="${HOME}/.emacs.d"
    export CMACSD="${HOME}/.cmacs.d"
    export TMPD="${HOME}/tmp"
    export GOENV_ROOT="${HOME}/.goenv"
    export GOPATH="${HOME}/go"
    export PYENV_ROOT="${HOME}/.pyenv"
    export RBENV_ROOT="${HOME}/.rbenv"
    export NVM_DIR="${HOME}/.nvm"
    export NPM_ROOT="${HOME}/.npm"
    export CARGO_HOME="${HOME}/.cargo"
    find "${DOTFILES_ROOT}/bin/z" -type f | sort | while read line ; do source "$line" ; done
}
