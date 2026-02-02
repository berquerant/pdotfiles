#!/bin/bash

readonly path="$1"
readonly ext="$2"
readonly pattern="${3:-.*}"

if [[ -z "$path" ]] ; then
    echo >&2 "path(arg0) is required"
    exit 1
fi
if [[ -z "$ext" ]] ; then
    echo >&2 "ext(arg1) is required"
    exit 1
fi

dasel query -i "$ext" -o json < "$path" | gron | grep -E "$pattern"
