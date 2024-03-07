#!/bin/bash

__check_lines() {
    extension_regex="$1"
    threshold="$2"
    git ls-files | sort | grep "$extension_regex" | while read p ; do
        echo "[check_lines] ${p}"
        l="$(wc -l $p|awk '{print $1}')"
        if [ "$l" -gt "$threshold" ] ; then
            echo "${p} too large, ${l} lines > ${threshold}"
            return 1
        fi
    done
}

check_zsh_lines() {
    threshold="$1"
    __check_lines '\.zsh$' "$threshold"
}

check_bash_lines() {
    threshold="$1"
    __check_lines '\.sh$' "$threshold"
}


set -e
check_bash_lines "${BASH_LINES_THRESHOLD:-200}"
check_zsh_lines "${ZSH_LINES_THRESHOLD:-100}"
