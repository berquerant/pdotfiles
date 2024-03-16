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

update_package() {
    pkg="$1"
    if [ -z "$pkg" ] ; then
        echo "no package specified"
        return 1
    fi
    __batch --eval "(my-external-straight-update-package \"${pkg}\")" &&\
        __batch --eval '(my-external-straight-freeze)'
}

usage() {
    name="${0##*/}"
    cat - <<EOS
${name} -- package update utilities

Usage
  ${name} ls|list ...
    n|name|names
      List packages names.

    d|dir|directories
      List package directories.

  ${name} d|desc|describe ...
    a|all
      List packages details.

    DAYS
      List old packages details.
      DAYS is an integer, threshold for considering a package old.

  ${name} u|up|update PACKAGE
      Update package.
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
        *)
            usage
            return 1
            ;;
    esac
}

main "$@"
