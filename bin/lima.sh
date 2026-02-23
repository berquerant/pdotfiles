#!/bin/bash

# start VM with default settings

readonly d="$(cd "$(dirname "$0")"/.. || exit 1; pwd)"
readonly default_tmpl="${d}/lima-default.yml"

fzf() {
    "${d}/bin/fzf.sh"
}

templates() {
    limactl start --list-templates
}

render() {
    local -r __tmpl="$1"
    local -r __dest="$2"
    sed "s|TEMPLATE|${__tmpl}|" "$default_tmpl" > "$__dest"
}

readonly name="$1"
shift
if [[ -z "$name" ]] ; then
    echo >&2 "NAME(arg0) is required"
    exit 1
fi

set -e
set -o pipefail
tmp="$(mktemp)"
# shellcheck disable=SC2064
trap "rm -f ${tmp}" EXIT
templates | fzf | while read -r tmpl ; do
    render "$tmpl" "$tmp"
    cat "$tmp"
    limactl start -y --name "$name" "$tmp" "$@"
done
