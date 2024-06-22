#!/bin/bash

d=$(cd $(dirname $0)/..; pwd)
. "${d}/bin/cache.sh"

__default_branch_cache="${TMPD}/cache/default_branch"
mkdir -p "$(dirname $__default_branch_cache)"

__default_branch() {
    git remote show origin | grep -F 'HEAD branch:' | cut -d ':' -f 2 | tr -d ' '
}

default_branch() {
    __default_branch_key="$PWD"
    __default_branch_value="$(cache_get "$__default_branch_cache" "$__default_branch_key")"
    if [ -n "$__default_branch_value" ] ; then
        echo "$__default_branch_value"
        return
    fi
    __default_branch_value="$(__default_branch)"
    # ttl is 1 day
    cache_set "$__default_branch_cache" "$__default_branch_key" "$__default_branch_value" 86400
    echo "$__default_branch_value"
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
