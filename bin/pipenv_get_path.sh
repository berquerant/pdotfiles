#!/bin/bash

set -e

if [ "$EDITOR" = "$0" ] ; then
    if echo "$@" | grep -q "/" ; then
        # pipenv open calls EDITOR with abs path of file as argument
        # echo it
        echo "$@"
    else
        # pipenv open with myself
        pipenv open --quiet "$@" | grep "^/"
    fi
else
    # Set EDITOR to myself and call myself
    EDITOR="$0" $0 "$@"
fi
