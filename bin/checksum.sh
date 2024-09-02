#!/bin/bash

set -e
set -o pipefail

hash_cmd="${HASH:-sha256sum}"

hash_dir() {
    find "$1" -type f -print0 |\
        sort --zero-terminated |\
        xargs -0 "$hash_cmd" |\
        "$hash_cmd" |\
        awk -v f="$1" '{printf "%s  %s\n", $1, f}'
}

hash_select() {
    local target="$1"
    if [ -z "$target" ] ; then
        # stdin
        "$hash_cmd"
    elif [ -d "$target" ] ; then
        hash_dir "$target"
    else
        "$hash_cmd" "$target"
    fi
}

if [ "$1" = "-h" ] || [ "$1" = "--help" ] ; then
    name="${0##*/}"
    cat - <<EOS > /dev/stderr
${name} [TARGET...]
EOS
    exit
fi

if [ -z "$1" ] ; then
    hash_select
else
    while [ -n "$1" ] ; do
        hash_select "$1"
        shift
    done
fi
