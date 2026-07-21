#!/bin/bash

__default_branch() {
  git default-branch
}

default_branch() {
  __default_branch "$PWD" 86400
}

switch_branch() {
  local __db
  __db="$(default_branch)"
  if [ "$__db" = "$(git current-branch)" ] ; then
    return
  fi
  git switch "$__db" "$@"
}

switch_pull_branch() {
  local -r cleanup="$1"
  local -r switch_back="$2"
  local current
  current="$(git current-branch)"
  switch_branch -q
  local __db
  __db="$(default_branch)"
  if [[ "$cleanup" == "true" && "$__db" != "$current" ]] ; then
    git branch -D "$current"
  fi
  git pull
  if [[ "$switch_back" == "true" && "$__db" != "$current" ]] ; then
    git switch -
  fi
}

remove_merged_branches() {
  local -r switch_back="$1"
  local current
  current="$(git current-branch)"
  switch_branch -q
  git pull
  local __db
  __db="$(default_branch)"
  git branch --merged | grep -vF "$__db" | while read -r x ; do
    echo "Delete branch: ${x}"
    git branch -d "$x"
  done
  if [[ "$switch_back" == "true" && "$__db" != "$current" ]] ; then
    git switch - || true
  fi
}

force_branch() {
  local current
  current="$(git current-branch)"
  local -r branch="${1:-$current}"
  local __db
  __db="$(default_branch)"
  if [[ "$branch" == "$__db" ]] ; then
    return 1
  fi
  git switch "$__db"
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

  ${name} c|cleanup [SWITCH_BACK]
    Remove all merged local branches
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
  "c" | "cleanup") shift ; remove_merged_branches "$@" ;;
  "b" | "branch") shift ; force_branch "$@" ;;
  "-h" | "--help") usage ; exit 1 ;;
  *) default_branch ;;
esac
