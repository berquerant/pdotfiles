#!/bin/bash

usage() {
    name="${0##*/}"
    cat - <<EOS > /dev/stderr
${name} SRC DST [DST...]

make symlink from SRC to DST.

You may use directories or relative paths that do not exist in DST.
EOS
}

if [ -z "$1" ] ; then
    echo "Missing SRC" 2>&1
    exit 1
fi

readonly src="$1"
shift

if [ -z "$1" ] ; then
    echo "Missing DST" 2>&1
    exit 1
fi

__link() {
    local -r _src="$1"
    local -r _dst="$2"

    local -r _abs_src="$(grealpath "$_src")"
    local -r _base_dst="$(basename "$_dst")"
    local -r _dir_dst="$(dirname "$_dst")"

    mkdir -p "$_dir_dst"
    pushd "$_dir_dst" >/dev/null 2>&1
    local -r _rel_src="$(grealpath --relative-to=. "$_abs_src")"
    ln -sv "$_rel_src" "$_base_dst"
    popd >/dev/null 2>&1
}


set -e
for dst in "$@" ; do
    __link "$src" "$dst"
done
