#!/bin/bash

d=$(cd $(dirname $0)/..; pwd)
. "${d}/bin/common.sh"

__run() {
    cecho green "$*"
    if [ -z "$DEBUG" ] ; then
        "$@"
    fi
}

clean_docker() {
    __run docker system prune --force --filter 'until=168h'
    __run docker volume prune --force
}

clean_brew() {
    __run brew cleanup --prune 7
}

clean_go() {
    goenv versions | grep -vE "${GO_VERSION}|system" | while read version ; do
        cecho yellow "UNINSTALL go ${version}"
        __run goenv uninstall -f "$version"
    done
    find "$GOPATH" -type d -maxdepth 1 -depth 1 | grep -v -E "(${GO_VERSION}|src)$" | sort | while read x ; do
        cecho yellow "DELETE ${x}"
        __run sudo rm -rf "$x"
    done
}

clean_python() {
    pyenv versions | grep -vE "${PY_VERSION}|system" | while read version ; do
        cecho yellow "UNINSTALL python ${version}"
        __run pyenv uninstall -f "$version"
    done
}

clean_ruby() {
    rbenv versions | grep -vE "${RB_VERSION}|system" | while read version ; do
        cecho yellow "UNINSTALL ruby ${version}"
        __run rbenv uninstall -f "$version"
    done
}

clean_node() {
    . "${NVM_DIR}/nvm.sh"
    nvm ls --no-colors | grep -vE "${NODE_VERSION}|default|N/A" | awk '{print $1}' | while read version ; do
        cecho yellow "UNINSTALL node ${version}"
        __run nvm uninstall "$version"
    done
}

clean_etc() {
    __run rm -rf "$TMPD"
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
        cecho green "TARGET: go|python|ruby|node|brew|docker|etc"
        cecho green "dryrun if envvar DEBUG is not empty"
        return 1
    fi

    args="$*"
    if [ "$1" = "all" ] ; then
        args="go python ruby node brew docker etc"
    fi
    for arg in $args ; do
        run "$arg"
    done
}

set -e
main "$@"
