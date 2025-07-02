#!/bin/bash

diffcmd() {
    # default: k8s-object-diff-go
    ${DIFF:-kd -c} "$@"
}

rootd() {
    local -r _rootd="${ROOT:-$TMPD/.k8s-diff-sync}"
    mkdir -p "$_rootd"
    echo "$_rootd"
}

kget() {
    kubectl get -o yaml "$@" | yq '.items[] | split_doc'
}

_destname() {
    local -r _prefix="$1"
    local -r _target="$2"
    shift 2
    local _root
    _root="$(rootd)"
    echo "${_root}/${_prefix}-${_target}.yml"
}

_save() {
    local -r _prefix="$1"
    local -r _target="$2"
    shift 2
    local _dest
    _dest="$(_destname "$_prefix" "$_target")"
    kget "$_target" "$@" > "$_dest"
    echo >&2 "Saved to ${_dest}"
}

save() {
    _save "before" "$@"
}

diff_sync() {
    local -r _target="$1"
    shift
    local _root
    _root="$(rootd)"
    local _src
    _src="$(_destname "before" "$_target")"
    if [[ ! -s "$_src" ]] ; then
        save "$_target" "$@"
    else
        _save "after" "$_target" "$@"
        local _dest
        _dest="$(_destname "after" "$_target")"
        echo >&2 "Diff between ${_src} and ${_dest}"
        diffcmd "$_src" "$_dest"
    fi
}

usage() {
    local -r _name="${0##*/}"
    cat >&2 <<EOS
${_name} OPERATION TARGET [KUBECTL_GET_OPTION...]

${_name} (save|s) TARGET [KUBECTL_GET_OPTION...]
  Save the TARGET as a 'before' file.

${_name} (diff|d) TARGET [KUBECTL_GET_OPTION...]
  Save the TARGET as a 'after' file and compare 'before' and 'after'.
  Fallback to 'save' if 'before' file not exists.
EOS
}

readonly op="$1"
readonly target="$2"
shift
if [[ "$op" == "--help" || "$op" == "-h" ]] ; then
    usage
    exit
fi
if [[ -z "$op" || -z "$target" ]] ; then
    usage
    exit 1
fi

set -e
case "$op" in
    "save" | "s") save "$@" ;;
    "diff" | "d") diff_sync "$@" ;;
    *)
        usage
        exit 1
        ;;
esac
