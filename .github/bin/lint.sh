#!/bin/bash

d=$(cd "$(dirname "$0")" || exit; pwd)
. "${d}/common.sh"

__check() {
    local file="$1"
    local threshold="$2"
    echo "[check] ${1}"
    local lines
    lines="$(wc -l "$file" | awk '{print $1}')"
    if [ "$lines" -gt "$threshold" ] ; then
       echo "${file} too large, ${lines} lines > ${threshold}"
       return 1
    fi
}

check() {
    find_by_interpreter "$1" "$2" | while read -r line ; do
        __check "$line" "$3"
    done
}

set -e
check bash sh "${BASH_LINES_THRESHOLD:-200}"
check zsh zsh "${ZSH_LINES_THRESHOLD:-100}"
