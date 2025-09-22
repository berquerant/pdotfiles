#!/bin/bash

readonly root="${DOTFILES_ROOT}/ivg"
readonly repo="${root}/repos"
readonly locks="${root}/locks"
readonly lock_file="${root}/renovate.lock"


generate() {
    rnv "$repo" batch gen > "$lock_file"
}

apply() {
    local id
    local commit
    rnv "$repo" batch lock < "$lock_file" | while read -r line ; do
        id="$(echo "$line" | cut -d ' ' -f 1)"
        commit="$(echo "$line" | cut -d ' ' -f 2)"
        echo "$commit" > "${locks}/${id}.lock"
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
