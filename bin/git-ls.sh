#!/bin/bash

select_paths() {
    git ls-files --deduplicate | "${DOTFILES_ROOT}/bin/fzf.sh"
}

if [[ -z "$1" ]] ; then
    select_paths
else
    set -eo pipefail
    select_paths | while read -r x ; do "$@" "$x" ; done
fi
