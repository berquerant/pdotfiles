#!/bin/bash

readonly d="$(cd "$(dirname "$0")" || exit; pwd)"

export DOCKER_MAN_DEBIAN_PORT="30180"
export DOCKER_MAN_UBUNTU_PORT="30181"

compose() {
    pushd "${d}/dman" >/dev/null 2>&1
    docker compose "$@"
    popd >/dev/null 2>&1
}

health_check() {
    local -r _os="$1"
    "${DOTFILES_ROOT}/bin/healthcheck.sh" "http://localhost:$(__os2port "$__os")/"
}

build_url() {
    local -r _os="$1"
    local -r _page_or_section="$2"
    local -r _page="$3"

    local -r _man_path="/cgi-bin/man/man2html"
    if [ -z "$_page_or_section" ] ; then
        echo "http://localhost:$(__os2port "$_os")${_man_path}"
        return
    fi
    if [ -z "$_page" ] ; then
        echo "http://localhost:$(__os2port "$_os")${_man_path}?${_page_or_section}"
        return
    fi
    echo "http://localhost:$(__os2port "$_os")${_man_path}?${_page_or_section}+${_page}"
}

__os2port() {
    case "$1" in
        debian) echo "$DOCKER_MAN_DEBIAN_PORT" ;;
        ubuntu) echo "$DOCKER_MAN_UBUNTU_PORT" ;;
        *) return 1 ;;
    esac
}

__help() {
    local -r name="${0##*/}"
    cat <<EOS >&2
${name} -- manual via docker-man

Usage
  ${name} (compose|c) DOCKER_COMPOSE_ARGS...
    run docker compose

  ${name} (start|up)
    launch docker-man containers

  ${name} -h
    show this help

  ${name} OS PAGE
    man PAGE of OS

  ${name} OS SECTION PAGE
    man SECTION PAGE of OS
EOS
}

set -e
case "$1" in
    compose | c)
        shift
        compose "$@"
        ;;
    start | up)
        compose up -d
        ;;
    -h)
        __help
        ;;
    *)
        readonly __os="$1"
        readonly __page_or_section="$2"
        readonly __page="$3"
        compose up -d
        health_check "$__os"
        readonly __url="$(build_url "$__os" "$__page_or_section" "$__page")"
        open "$__url"
esac
