#!/bin/bash

latest_container() {
    docker ps --latest --format json | jq -r .ID
}

select_container() {
    docker ps | awk 'NR>1' | "${DOTFILES_ROOT}/bin/fzf.sh" | cut -d' ' -f1
}

commit_container() {
    local -r container_id="$(select_container)"
    read -r "image_name?name>"
    # shellcheck disable=SC2154
    docker commit "$container_id" "$image_name"
}

exec_container() {
    local -r container_id="$(select_container)"
    local args=()
    local replaced=0
    for arg in "$@" ; do
        if [[ "$replaced" == 0 && "$arg" == "--" ]] ; then
            replaced=1
            args+=("$container_id")
            continue
        fi
        args+=("$arg")
    done
    set -x
    docker exec "${args[@]}"
}

run_with_pwd() {
    set -x
    docker run --rm -v "${PWD}:/usr/src/app" -w "/usr/src/app" "$@"
}

layers() {
    docker history --no-trunc --format json "$@"
}

__cmdcomp() {
    cmdcomp -x 'diff -u --color=always' "$@"
}
__cmdcomp_jq() {
    local -r __cmd="$1"
    local -r __left="$2"
    local -r __right="$3"
    shift 3
    # shellcheck disable=SC2086
    __cmdcomp -p 'jq .' "$@" -- $__cmd -- "$__left" -- "$__right"
}
manifest_diff() {
    __cmdcomp_jq "crane manifest" "$@"
}
config_diff() {
    __cmdcomp_jq "crane config" "$@"
}
file_diff() {
    local -r __left="$1"
    local -r __right="$2"
    shift 2
    __cmdcomp -p 'tar -tvf -' -p 'sort' "$@" -- crane export -- "$__left" -- "$__right"
}

usage() {
    local -r name="${0##*/}"
    cat <<EOS >&2
${name} -- docker utils

Usage
  ${name} latest
    Display the latest created container id
  ${name} (s|select)
    Select running container id
  ${name} (c|commit)
    Commit container image
  ${name} (x|exec) [EXEC_OPTS] -- CMD
    Execute command in container
  ${name} (r|run) [RUN_ARGS]
    Run container with bind mount PWD
  ${name} (l|layer) IMAGE
    Display image layers
  ${name} (md|mdiff) IMAGE1 IMAGE2 [CMD_COMP_OPT...]
    Display manifest diff
  ${name} (cd|cdiff) IMAGE1 IMAGE2 [CMD_COMP_OPT...]
    Display config diff
  ${name} (fd|fdiff) IMAGE1 IMAGE2 [CMD_COMP_OPT...]
    Display file diff
EOS
}

cmd=""
case "$1" in
    latest) cmd="latest_container" ;;
    s|select) cmd="select_container" ;;
    c|commit) cmd="commit_container" ;;
    x|exec) cmd="exec_container" ;;
    r|run) cmd="run_with_pwd" ;;
    l|layer) cmd="layers" ;;
    md|mdiff) cmd="manifest_diff" ;;
    cd|cdiff) cmd="config_diff" ;;
    fd|fdiff) cmd="file_diff" ;;
    *) usage ; exit 1 ;;
esac
shift
set -e
"$cmd" "$@"
