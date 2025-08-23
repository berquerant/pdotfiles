#!/bin/zsh

zload() {
    source "${DOTFILES_ROOT}/.env"
    "${DOTFILES_ROOT}/bin/load-envdir.sh"
    find "${DOTFILES_ROOT}/bin/z" -type f | sort | while read line ; do source "$line" ; done
}
