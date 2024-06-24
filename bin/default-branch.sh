#!/bin/bash

d=$(cd $(dirname $0)/..; pwd)
. "${d}/bin/cache.sh"

__default_branch() {
    git remote show origin | grep -F 'HEAD branch:' | cut -d ':' -f 2 | tr -d ' '
}

default_branch() {
    cache_function __default_branch "$PWD" 86400
}

switch_branch() {
    if [ "$(default_branch)" = "$(current_branch)" ] ; then
        return
    fi
    git switch "$(default_branch)" "$@"
}

diff_branch() {
    git diff "$(default_branch)"
}

current_branch() {
    git branch --contains | awk '$1=="*"{print $2}'
}

switch_pull_branch() {
    cleanup="$1"
    if [ -z "$cleanup" ] ; then
        switch_branch -q
    else
        current="$(current_branch)"
        switch_branch -q
        if [ "$(default_branch)" != "$current" ] ; then
            git branch -D "$current"
        fi
    fi
    git pull -q
}

usage() {
    name="${0##*/}"
    cat - <<EOS >&2
${name} -- git with default branch

Usage
  ${name}
    Display default branch

  ${name} s|switch
    Switch to default branch

  ${name} d|diff
    Git diff with default branch

  ${name} p|pull [CLEANUP]
    Switch to default branch and pull
    If CLEANUP, delete the branch before switching
EOS
}

set -e
case "$1" in
    "s" | "switch")
        switch_branch
        ;;
    "d" | "diff")
        diff_branch
        ;;
    "p" | "pull")
        shift
        switch_pull_branch "$@"
        ;;
    "-h" | "--help")
        usage
        exit 1
        ;;
    *)
        default_branch
        ;;
esac
