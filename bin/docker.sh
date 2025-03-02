#!bin/bash

latest_container() {
    docker ps --latest --format json | jq -r .ID
}

select_container() {
    docker ps | awk 'NR>1' | peco | cut -d' ' -f1
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
EOS
}


cmd=""
case "$1" in
    latest) cmd="latest_container" ;;
    s|select) cmd="select_container" ;;
    c|commit) cmd="commit_container" ;;
    x|exec) cmd="exec_container" ;;
    r|run) cmd="run_with_pwd" ;;
    *)
        usage
        exit 1
        ;;
esac
shift

set -e
"$cmd" "$@"
