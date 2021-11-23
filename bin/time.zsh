#!/bin/zsh

export DATETIME_FORMAT="%Y-%m-%d %H:%M:%S"

# print datetime as human readable format
# datetime [TIMESTAMP]
datetime() {
    if [ $# -eq 0 ]
    then
        date "+$DATETIME_FORMAT"
    else
        date -r "$1" "+$DATETIME_FORMAT"
    fi
}

# print timestamp
# timestamp [DATETIME]
timestamp() {
    if [ $# -eq 0 ]
    then
        date +%s
    else
        date -j -f "$DATETIME_FORMAT" "$1" +%s
    fi
}

# print datetime sequence
# dateseq START_DATETIME N UNIT
# UNIT: y|m|w|d|H|M|S cf. man date
dateseq() {
    local unit="d"
    if [ $# -ge 3 ]
    then
        unit="$3"
    fi
    seq 0 $(expr "$2" - 1) | xargs -I N date -j -v+N"$unit" -f "$DATETIME_FORMAT" "$1" +"$DATETIME_FORMAT"
}
