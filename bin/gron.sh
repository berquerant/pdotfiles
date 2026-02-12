#!/bin/bash

readonly ext="$1"
readonly pattern="${2:-.*}"

if [[ -z "$ext" ]] ; then
    echo >&2 "ext(arg0) is required"
    exit 1
fi

dasel query -i "$ext" -o json | gron | grep -E "$pattern"
