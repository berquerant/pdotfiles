#!/bin/bash

__jmerge() {
    if [ $# = 0 ] ; then
        echo "jmerge FILE [FILE...]"
        echo "merge json files"
        return
    fi

    query='.[0]'
    if [ "$#" -gt 1 ] ; then
        m="$(($# - 1))"
        for i in `seq 1 $m` ; do
            query="${query}*.[${i}]"
        done
    fi

    jq --slurp "${query}" $*
}

__ymerge() {
    if [ $# = 0 ] ; then
        echo "ymerge FILE [FILE...]"
        echo "merge yaml files"
        return
    fi

    m="$(($# - 1))"
    for i in `seq 0 $m` ; do
        f="$(mktemp)"
        yq -ojson $1 > "$f"
        shift
        fs[$i]="$f"
    done
    __jmerge ${fs[@]} | cv json yml
}


t="$1"
shift
case "$t" in
    y | yml | yaml)
        __ymerge $@
        ;;
    j | json)
        __jmerge $@
        ;;
esac
