#!/bin/bash

ggdo() {
    repo_regex="$1"
    shift
    ghq list -p | rg "$repo_regex" | while read line ; do
        pushd "$line" > /dev/null
        if [ -n "$GGDO_RAW" ] ; then
            "$@"
        else
            bash -c "$@"
        fi
        popd > /dev/null
    done
}

__gggrep_run() {
    groot="$(ghq root)/"
    sed_expr="s|${groot}||"
    repo_path="$(pwd|sed $sed_expr)"
    git grep -H "$@" | awk -v r="$repo_path" '{print r"/"$0}'
}

gggrep() {
    repo_regex="$1"
    shift
    GGDO_RAW=1 ggdo "$repo_regex" __gggrep_run "$@"
}

usage() {
    name="${0##*/}"
    cat - <<EOS
${name} -- run commands on multiple repositories

Usage
  ${name} do REPO_REGEX ...
    Run command on multiple repositories.

  ${name} grep REPO_REGEX ...
    Grep multiple repositories
EOS
}

main() {
    cmd="$1"
    shift
    case "$cmd" in
        "do")
            ggdo "$@"
            ;;
        "grep")
            gggrep "$@"
            ;;
        *)
            usage
            return 1
            ;;
    esac
}

main "$@"
