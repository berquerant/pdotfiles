#!/bin/bash

set -e
set -o pipefail

readonly root="${DOTFILES_ROOT}/ivg"
readonly repos="${root}/repos"
readonly locks="${root}/locks"
readonly lock_file="${root}/renovate.lock"


generate() {
    rnv "$repos" batch gen > "$lock_file"
}

apply() {
    cut -d " " -f 1 "$lock_file" | cut -d "=" -f 2 | while read -r id ; do
        rnv "${repos}/${id}" lock < "$lock_file" > "${locks}/${id}.lock"
    done
}

usage() {
    local -r name="${0##*/}"
    cat <<EOS >&2
${name} -- renovate ivg apps

Usage:
  ${name} (g|gen|generate)
    Generate ${lock_file}
  ${name} (l|lock|apply)
    Update ivg locks
EOS
}

readonly cmd="$1"
case "$cmd" in
    g | gen | generate) generate ;;
    l | lock | apply ) apply ;;
    *)
        usage
        exit 1
        ;;
esac
