#!/bin/bash

#
# ./emacs-command-log-stat.sh [BEGIN_TIME] [END_TIME]
# e.g. ./emacs-command-log-stat.sh '2025-01-01 00:00:00'
#

readonly begin="$1"
readonly end="$2"

readonly hist="$EMACS_HISTFILE"

timestamp() {
    date -j -f "$DATETIME_FORMAT" "$1" +%s
}

filter_by_timerange() {
    if [[ -z "$begin" ]] ; then
        cat "$hist"
    elif [[ -z "$end" ]] ; then
        local -r x="$(timestamp "$begin")"
        awk -v x="$x" 'BEGIN {w=0} w || $1 >= x {w=1;print}' "$hist"
    else
        local -r x="$(timestamp "$begin")"
        local -r y="$(timestamp "$end")"
        awk -v x="$x" 'BEGIN {w=0} w || $1 >= x {w=1;print}' "$hist" |\
            awk -v y="$y" '$1 > y {exit} {print}'
    fi
}

sanitize() {
    awk '$2 ~ /^[a-zA-Z]/'
}

reduce() {
    awk '{s[$2] += $3} END {for(k in s) print s[k]","k}' |\
        awk -F"," '$1 > 1' |\
        sort -nrk 1 -t","
}

filter_by_timerange | sanitize | reduce
