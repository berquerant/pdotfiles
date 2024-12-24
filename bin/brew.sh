#!/bin/bash

list() {
    brew leaves -r
    brew list --cask | xargs | tr " " "\n"
}

deps() {
    case "$1" in
        "r")
            brew uses --installed "$2"
            ;;
        "")
            brew deps --tree --installed
            ;;
        *)
            brew deps --tree "$1"
            ;;
    esac
}

readonly cmd="$1"
shift
case "$cmd" in
    ls | list)
        list
        ;;
    dep | deps)
        deps "$@"
        ;;
    *)
        cat << EOS > /dev/stderr
$0 list
$0 deps [FORMULA]
$0 deps r [FORMULA]
EOS
        exit 1
        ;;
esac
