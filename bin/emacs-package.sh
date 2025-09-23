#!/bin/bash

d="$(cd "$(dirname "$0")"/.. || exit; pwd)"
. "${d}/bin/cache.sh"

__batch() {
    "${d}/bin/emacs-batch.sh" "$@"
}

__batch_cache() {
    cache_function_args __batch "$@"
}

__grinfo_cache() {
    cache_function_io_args grinfo --worker 8
}

straight_info() {
    __batch_cache --eval '(my-external-straight-info)'
}

describe_packages() {
    straight_info | jq -r '.directories[]' | __grinfo_cache
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
    straight_info | jq -c .dependencies | __render_deps
}

render_dependents() {
    straight_info | jq -c .dependents | __render_deps
}

usage() {
    name="${0##*/}"
    cat - <<EOS >&2
${name} -- package info utilities

Usage
  ${name}
    Display package info.
  ${name} desc
    Discribe packages details.
  ${name} dependencies
    Render dependencies graph (json2dot).
  ${name} dependents
    Render dependents graph (json2dot).
EOS
}

main() {
    local -r cmd="$1"
    shift
    case "$cmd" in
        "-h" | "--help") usage ;;
        "desc" | "describe") describe_packages ;;
        "dependencies") render_dependencies ;;
        "dependents") render_dependents ;;
        "") straight_info ;;
        *)
            usage
            exit 1
            ;;
    esac
}

main "$@"
