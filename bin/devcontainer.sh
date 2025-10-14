#!/bin/bash

__up() {
    devcontainer up --workspace-folder . "$@"
}

__exec() {
    devcontainer exec --workspace-folder . "$@"
}

__up_result2path() {
    jq '"/docker:" + .containerId + ":" + .remoteWorkspaceFolder + "/"' -r
}

up() {
    local -r target="${EMACS_OPEN_FILE_TARGET:-.emacs-open-file-target}"
    __up "$@" | __up_result2path > "$target"
    echo >&2 "Call my-open-file-find to open the file"
    echo >&2 "Path: $(cat "$target")"
}

down() {
    docker ps --format='{{.Image}} {{.ID}}' |\
        grep -E '^vsc-' |\
        "${DOTFILES_ROOT}/bin/fzf.sh" |\
        awk '{print $2}' |\
        xargs -n 1 docker rm -f
}

restart() {
    down || true
    up "$@"
}

set -e
set -o pipefail

readonly cmd="$1"
shift
case "$cmd" in
    up) up "$@" ;;
    down) down ;;
    restart) restart "$@" ;;
    exec) __exec "$@" ;;
    *)
        readonly name="${0##*/}"
        cat - <<EOS
${name} -- manage devcontainers

${name} up ...
${name} down
${name} restart ...
${name} exec ...
EOS
        exit 1
        ;;
esac
