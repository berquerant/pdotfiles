#!/bin/zsh

zload() {
    source "${DOTFILES_ROOT}/.env"
    find "${DOTFILES_ROOT}/bin/z" -type f | sort | while read line ; do source "$line" ; done
}
