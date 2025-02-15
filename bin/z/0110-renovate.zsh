#!/bin/zsh

alias renovate-dry='${DOTFILES_ROOT}/bin/renovate-dry.sh'

renovate-validate() {
    RENOVATE_CONFIG_FILE="${1:-renovate.json}" renovate-config-validator --strict
}
