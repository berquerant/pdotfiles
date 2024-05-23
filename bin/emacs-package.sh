#!/bin/bash

d=$(cd $(dirname $0)/..; pwd)

__batch() {
    "${d}/bin/emacs-batch.sh" "$@"
}

list_names() {
    __batch --eval '(my-external-straight-list-packages)'
}

list_directories() {
    __batch --eval '(my-external-straight-list-directories)'
}

describe_packages() {
    list_directories | grinfo --worker 8
}

describe_old_packages() {
    threshold_day="${1:-90}"
    describe_packages |\
        jq --arg t "$threshold_day" 'select(.timediff.day > ($t|tonumber))|[.url, .timediff.day]|@csv' -r |\
        tr -d '"' |\
        sort -t "," -n -k 2
}

freeze() {
    __batch --eval '(my-external-straight-freeze)'
}

update_package() {
    pkg="$1"
    if [ -z "$pkg" ] ; then
        echo "no package specified"
        return 1
    fi
    __batch --eval "(my-external-straight-update-package \"${pkg}\")" &&\
        freeze
}

check_all() {
    __batch --eval '(my-exrernal-straight-check-all)'
}

list_dependencies() {
    __batch --eval '(my-external-straight-dependencies)'
}

list_dependents() {
    __batch --eval '(my-external-straight-dependents)'
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
    cat "$EMACS_HISTFILE" |\
        awk '{s[$2]+=$3}END{for(k in s)print s[k]"\t"k}' |\
        sort -nk 1
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
    cmd="$1"
    shift
    case "$cmd" in
        "ls" | "list")
            cmd="$1"
            shift
            case "$cmd" in
                "n" | "name" | "names")
                    list_names
                    ;;
                "d" | "dir" | "directories")
                    list_directories
                    ;;
                "p" | "dep" | "dependencies")
                    list_dependencies
                    ;;
                "q" | "det" | "dependents")
                    list_dependents
                    ;;
                *)
                    usage
                    return 1
                    ;;
            esac
            ;;
        "d" | "desc" | "describe")
            cmd="$1"
            shift
            case "$cmd" in
                "a" | "all")
                    describe_packages
                    ;;
                *)
                    describe_old_packages "$@"
                    ;;
            esac
            ;;
        "u" | "up" | "update")
            update_package "$@"
            ;;
        "c" | "check")
            check_all
            ;;
        "r" | "render")
            cmd="$1"
            shift
            case "$cmd" in
                "d" | "dep" | "dependencies")
                    render_dependencies
                    ;;
                "q" | "det" | "dependents")
                    render_dependents
                    ;;
                *)
                    usage
                    return 1
                    ;;
            esac
            ;;
        "hs" | "histstat")
            histfile_stat
            ;;
        "f" | "freeze")
            freeze
            ;;
        *)
            usage
            return 1
            ;;
    esac
}

main "$@"
