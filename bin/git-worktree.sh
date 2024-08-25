#!/bin/bash

default_root="${GHT_ROOT}/git-worktree"
root="${GIT_WORKTREE_ROOT:-$default_root}"

repopath() {
    git config --get remote.origin.url |\
        tr ":" "/" |\
        sed -E 's|git@|https///|' |\
        sed -E 's|git///|https///|' |\
        sed -E 's|\.git||' |\
        sed -E 's|https///|https://|' |\
        sed -E 's|https://||'
}

add() {
    local branch="$1"
    if [ -z "$branch" ] ; then
        return 1
    fi

    worktree_prefix="${GIT_WORKTREE_ROOT}/$(repopath)"
    worktree_path="${worktree_prefix}/${branch}"
    echo "${worktree_path}"
    git worktree add "${worktree_path}" "$@"
}

remove() {
    git worktree remove "$@"
}

list() {
    git worktree list "$@"
}

usage() {
    local name="${0##*/}"
    cat - <<EOS >&2
${name} -- git worktree wrapper

Usage
  ${name} [-h|--help]
    Show this help

  ${name} a|add BRANCH [OPTION]
    Add worktree under ${root}

  ${name} r|remove WORKTREE [OPTION]
    Remove worktree

  ${name} l|ls|list [OPTION]
   List worktrees

Environment variables
  GIT_WORKTREE_ROOT
    Directory to place worktrees
EOS
}


cmd="$1"
shift
case "$cmd" in
    "a" | "add")
        add "$@"
        ;;
    "r" | "remove")
        remove "$@"
        ;;
    "l" | "ls" | "list")
        list "$@"
        ;;
    *)
        usage
        exit 1
        ;;
esac
