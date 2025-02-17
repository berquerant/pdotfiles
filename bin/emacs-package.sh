#!/bin/bash

d="$(cd "$(dirname "$0")"/.. || exit; pwd)"
. "${d}/bin/cache.sh"

__emacs_package_batch() {
    "${d}/bin/emacs-batch.sh" "$@"
}

__batch() {
    __emacs_package_batch "$@"
}

__batch_cache() {
    cache_function_args __emacs_package_batch "$@"
}

__grinfo_cache() {
    cache_function_io_args grinfo --worker 8
}

list_names() {
    __batch_cache --eval '(my-external-straight-list-packages)'
}

list_directories() {
    __batch_cache --eval '(my-external-straight-list-directories)'
}

describe_packages() {
    list_directories | __grinfo_cache
}

read_profile() {
    __batch_cache --eval '(my-external-straight-read-profile)'
}

describe_old_packages() {
    local threshold_day="${1:-90}"
    describe_packages |\
        jq --arg t "$threshold_day" 'select(.timediff.day > ($t|tonumber))|[.url, .timediff.day]|@csv' -r |\
        tr -d '"' |\
        sort -t "," -n -k 2
}

freeze() {
    __batch_cache --eval '(my-external-straight-freeze)'
}

update_package() {
    local pkg="$1"
    if [ -z "$pkg" ] ; then
        echo "no package specified"
        return 1
    fi
    __batch --eval "(my-external-straight-update-package \"${pkg}\")" && freeze
}

check_all() {
    __batch_cache --eval '(my-exrernal-straight-check-all)'
}

list_dependencies() {
    __batch_cache --eval '(my-external-straight-dependencies)'
}

list_dependents() {
    __batch_cache --eval '(my-external-straight-dependents)'
}

__render_deps() {
    jq 'to_entries[]|select(.value != null)|[.key, (.value | if type == "object" then keys else . end | flatten | unique[])] | @csv' -r |\
        tr -d '"' |\
        awk -F "," '{
split($0, xs, ",");
from = xs[1];
for (i = 2; i <= length(xs); i++)
  printf("{\"src\":{\"id\":\"%s\"},\"dst\":{\"id\":\"%s\"}}\n", from, xs[i])}'
}

render_dependencies() {
    list_dependencies | __render_deps
}

render_dependents() {
    list_dependents | __render_deps
}

histfile_stat() {
    awk '{s[$2]+=$3}END{for(k in s)print s[k]"\t"k}' "$EMACS_HISTFILE" | sort -nk 1
}

describe_local() {
    local pkg="$1"
    if [ -n "$pkg" ] ; then
        return 1
    fi
    read_profile |\
        jq -c --arg name "$pkg" '{"profiles": [to_entries[] | select(.key | test($name))] | from_entries}'
    find "${EMACSD}/straight/build" -d 1 -maxdepth 1 -name "*${pkg}*" -type d |\
        jq -c --raw-input --slurp '{"build": [split("\n")[] | select(. != "")]}'
    find "${EMACSD}/straight/repos" -d 1 -maxdepth 1 -name "*${pkg}*" -type d | __grinfo_cache | jq -c
}

usage() {
    name="${0##*/}"
    cat - <<EOS >&2
${name} -- package update utilities

Usage
  ${name} ls|list ...
    n|name|names
      List packages names.

    d|dir|directories
      List package directories.

    p|dep|dependencies
      List package dependencies.

  ${name} d|desc|describe ...
    a|all
      List packages details.

    l|local NAME
      describe_local

    DAYS
      List old packages details.
      DAYS is an integer, threshold for considering a package old.

  ${name} u|up|update PACKAGE
      Update package.

  ${name} c|check
      Rebuild any packages that have been modified.

  ${name} r|render
    d|dep|dependencies
      Render dependencies graph.

    q|det|dependents
      Render dependents graph.

  ${name} hs|histstat
    Display hisrory stat.

  ${name} f|freeze
    Save installed packages.
EOS
}

main() {
    local cmd="$1"
    shift
    case "$cmd" in
        "ls" | "list")
            cmd="$1"
            shift
            case "$cmd" in
                "n" | "name" | "names") list_names ;;
                "d" | "dir" | "directories") list_directories ;;
                "p" | "dep" | "dependencies") list_dependencies ;;
                "q" | "det" | "dependents") list_dependents ;;
                *) usage ; return 1 ;;
            esac
            ;;
        "d" | "desc" | "describe")
            cmd="$1"
            shift
            case "$cmd" in
                "a" | "all") describe_packages ;;
                "l" | "local") describe_local "$@" ;;
                *) describe_old_packages "$@" ;;
            esac
            ;;
        "u" | "up" | "update") update_package "$@" ;;
        "c" | "check") check_all ;;
        "r" | "render")
            cmd="$1"
            shift
            case "$cmd" in
                "d" | "dep" | "dependencies") render_dependencies ;;
                "q" | "det" | "dependents") render_dependents ;;
                *) usage ; return 1 ;;
            esac
            ;;
        "hs" | "histstat") histfile_stat ;;
        "f" | "freeze") freeze ;;
        *) usage ;;
    esac
}

main "$@"
