#!/bin/bash

delete_cache() {
    local -r _d="$EMACSD"
    rm -rf "${_d}/eln-cache" "${_d}/.cache"
    find "$_d" -name "*.elc" -delete
    find "$_d" -name "*.eln" -delete
}

delete_straight_package() {
    local -r _d="$EMACSD"
    local -r _pkg="$1"

    local -r _sd="${_d}/straight"
    local -r _sdbackup="${_sd}.bk"
    if [ -z "$_pkg" ] ; then
        set -x
        rm -rf "$_sdbackup"
        mv "$_sd" "$_sdbackup"
        set +x
    else
        set -x
        rm -rf "${_sd}/build/${_pkg}" "${_sd}/modified/${_pkg}" "${_sd}/repos/${_pkg}"
        set +x
    fi
}

usage() {
    local -r name="${0##*/}"
    cat - <<EOS >&2
${name} c
Delete emacs caches.

${name} s [PACKAGE]
Delete straight package.
If no PACKAGE, delete whole straight packages.
EOS
}

set -e
cmd="$1"
case "$cmd" in
    "c" | "cache") cmd="delete_cache" ;;
    "s" | "straight" | "p" | "pkg" | "package") cmd="delete_straight_package" ;;
    "" | "-h" | "--help")
        usage
        exit
        ;;
    *)
        echo "unknown command: ${cmd}" >&2
        usage
        exit 1
        ;;
esac
shift
"$cmd" "$@"
