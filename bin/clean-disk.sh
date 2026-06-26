#!/bin/bash

d="$(cd "$(dirname "$0")"/.. || exit; pwd)"
. "${d}/bin/common.sh"

__run() {
    cecho green "$*"
    if [ -z "$DEBUG" ] ; then
        "$@"
    fi
}

clean_docker() {
    __run docker system prune -a --force --filter 'until=120h'
    __run docker builder prune -a --force --filter 'until=120h'
    __run docker volume prune -a --force
    __run docker system df -v
}

clean_brew() {
    __run brew cleanup --prune 5
}

clean_go() {
    du -sh "$(go env GOCACHE)"
    go clean -cache -testcache
}

clean_python() {
    pyenv versions | grep -vE "${PY_VERSION}|system" | while read -r version ; do
        cecho yellow "UNINSTALL python ${version}"
        __run pyenv uninstall -f "$version"
    done
    local -r venvs="${HOME}/.local/share/virtualenvs"
    find "$venvs" -depth 1 -delete | while read -r x ; do
        du -sh "$x"
        __run rm -rf "$x"
    done
    __run uv cache prune
}

clean_ruby() {
    echo >&2 "clean_ruby: noop"
}

clean_node() {
    . "${NVM_DIR}/nvm.sh"
    nvm ls --no-colors | grep -vE "${NODE_VERSION}|default|N/A" | awk '{print $1}' | while read -r version ; do
        cecho yellow "UNINSTALL node ${version}"
        __run nvm uninstall "$version"
    done
    # clean $NPM_ROOT/_cacache
    npm cache verify
}

clean_etc() {
    du -sh "$TMPD"
    __run rm -rf "$TMPD"
    mkdir -p "$TMPD"
}

run() {
    case "$1" in
        "go") clean_go ;;
        "python") clean_python ;;
        "ruby") clean_ruby ;;
        "node") clean_node ;;
        "brew") clean_brew ;;
        "docker") clean_docker ;;
        "etc") clean_etc ;;
        *) cecho yellow "Unknown target: $1" ;;
    esac
}

main() {
    if [ -z "$1" ] ; then
        cecho green "${0##*/} TARGET [TARGET...]"
        cecho green "TARGET: go|python|ruby|node|brew|docker|etc|all"
        cecho green "dryrun if envvar DEBUG is not empty"
        return 1
    fi

    local args="$*"
    if [ "$1" = "all" ] ; then
        args="go python ruby node brew docker etc"
    fi
    for arg in $args ; do
        run "$arg"
    done
}

set -e
main "$@"
