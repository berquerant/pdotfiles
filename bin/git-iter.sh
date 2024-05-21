#!/bin/bash

select_repos() {
    repo_regex="$1"
    ghq list -p | grep -E "$repo_regex"
}

__ggdo() {
    dir="$1"
    shift
    pushd "$dir" > /dev/null
    if [ -n "$GGDO_RAW" ] ; then
        "$@"
    else
        bash -c "$@"
    fi
    popd > /dev/null
}

ggdo() {
    repo_regex="$1"
    shift
    maxprocs="${GGDO_MAXPROCS:-1}"
    select_repos "$repo_regex" | xargs -n 1 -P "$maxprocs" -IT "$0" __ggdo T "$@"
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
    cat - <<EOS >&2
${name} -- run commands on multiple repositories

Usage
  ${name} do REPO_REGEX ...
    Run command on multiple repositories.

  ${name} grep REPO_REGEX ...
    Grep multiple repositories

Environment variables
  GGDO_MAXPROCS
    Maximum number of processes used for execution
    Default: 1
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
        "__ggdo")
            __ggdo "$@"
            ;;
        *)
            usage
            return 1
            ;;
    esac
}

main "$@"
