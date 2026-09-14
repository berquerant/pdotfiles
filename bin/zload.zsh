#!/bin/zsh

zload() {
    source "${DOTFILES_ROOT}/.env"
    export CMACSD="${HOME}/.cmacs.d"
    export TMPD="${HOME}/tmp"
    export GOPATH="${HOME}/go"
    export NPM_ROOT="${HOME}/.npm"
    export PNPM_HOME="${HOME}/.pnpm"
    export CARGO_HOME="${HOME}/.cargo"
    export PYENV_ROOT="${HOME}/.pyenv"
    find "${DOTFILES_ROOT}/bin/z" -type f | sort | while read line ; do source "$line" ; done
}
