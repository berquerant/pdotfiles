#!/bin/bash

uri2dir() {
    echo "$1" |\
        tr ":" "/" |\
        sed -E 's|git@|https///|' |\
        sed -E 's|git///|https///|' |\
        sed -E 's|\.git||' |\
        sed -E 's|https///|https://|' |\
        sed -E 's|https://||'
}

clone() {
    uri="$1"
    dir="$(ghq root)/$(uri2dir $uri)"
    if echo "$uri" | grep -q 'github.com' ; then
        ghq get "$uri"
    else
        git clone "$uri" "$dir"
    fi
    echo "$dir"
}

ghq_select() {
    ghq list -p | peco
}

select_clone() {
    uri="$1"
    if [ -z "$uri" ] ; then
        ghq_select
    else
        clone "$uri"
    fi
}

usage() {
    name="${0##*/}"
    cat - <<EOS >&2
${name} -- clone and change directory

Usage
  ${name}
    Select local repo by ghq and goto the repo directory

  ${name} URI
    Clone URI and goto the repo directory
EOS
}

if [ "$1" = "-h" ] || [ "$1" = "--help" ] ; then
    usage
    exit 1
fi

select_clone "$@"
