#!/bin/bash

d="$(cd "$(dirname "$0")"/.. || exit; pwd)"
. "${d}/bin/cache.sh"

__default_branch() {
    git default-branch
}

default_branch() {
    cache_function __default_branch "$PWD" 86400
}

switch_branch() {
    if [ "$(default_branch)" = "$(git current-branch)" ] ; then
        return
    fi
    git switch "$(default_branch)" "$@"
}

switch_pull_branch() {
    local -r cleanup="$1"
    local -r switch_back="$2"
    local current
    current="$(git current-branch)"
    switch_branch -q
    if [[ "$cleanup" == "true" && "$(default_branch)" != "$current" ]] ; then
        git branch -D "$current"
    fi
    git pull
    if [[ "$switch_back" == "true" && "$(default_branch)" != "$current" ]] ; then
        git switch -
    fi
}

force_branch() {
    local current
    current="$(git current-branch)"
    local -r branch="${1:-$current}"
    if [[ "$branch" == "$(default_branch)" ]] ; then
        return 1
    fi
    git switch "$(default_branch)"
    git branch -D "$branch" || true
    git fetch
    git pull
    git switch "$branch" || git checkout -b "$branch"
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

  ${name} p|pull [CLEANUP] [SWITCH_BACK]
    Switch to default branch and pull
    If CLEANUP is true, delete the branch before switching
    If SWITCH_BACK is true, switch back to the previous branch

  ${name} b|branch [BRANCH]
    Switch branch if remote branch exists else create forcely
    If BRANCH is empty, delete the current branch and create a new branch
EOS
}

set -e
case "$1" in
    "s" | "switch") switch_branch ;;
    "d" | "diff") git diff "$(default_branch)" ;;
    "p" | "pull") shift ; switch_pull_branch "$@" ;;
    "b" | "branch") shift ; force_branch "$@" ;;
    "-h" | "--help") usage ; exit 1 ;;
    *) default_branch ;;
esac
