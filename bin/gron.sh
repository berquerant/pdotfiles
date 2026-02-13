#!/bin/bash

readonly ext="$1"

if [[ -z "$ext" ]] ; then
    echo >&2 "ext(arg0) is required"
    exit 1
fi

readonly pattern="${2:-.*}"
readonly file="$3"

__dasel() {
    if [[ -n "$file" ]] ; then
        dasel query -i "$ext" -o json < "$file"
    else
        dasel query -i "$ext" -o json
    fi
}

__dasel | gron | grep -E "$pattern"
