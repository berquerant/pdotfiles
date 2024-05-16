#!/bin/bash

default_branch() {
    git remote show origin | grep -F 'HEAD branch:' | cut -d ':' -f 2 | tr -d ' '
}

switch_branch() {
    git switch "$(default_branch)"
}

diff_branch() {
    git diff "$(default_branch)"
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
EOS
}

case "$1" in
    "s" | "switch")
        switch_branch
        ;;
    "d" | "diff")
        diff_branch
        ;;
    "-h" | "--help")
        usage
        exit 1
        ;;
    *)
        default_branch
        ;;
esac
