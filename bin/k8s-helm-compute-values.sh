#!/bin/bash

set -e
set -o pipefail

usage() {
    local -r name="${0##*/}"
    cat <<EOS
${name} -- show final values

Usage:
  ${name} LOCAL_CHART_DIRECTORY [helm template options...]
EOS
}

case "$1" in
    "-h" | "--help")
        usage
        exit
        ;;
    "")
        usage
        exit 1
        ;;
esac

readonly chartd="$1"
shift

readonly templated="${chartd}/templates"
tmp_compute_yaml="$(mktemp -p "$templated")"
readonly compute_yaml="${tmp_compute_yaml}.yaml"
mv "$tmp_compute_yaml" "$compute_yaml"
# shellcheck disable=SC2064
trap "rm -f ${compute_yaml}" EXIT

echo '{{ toYaml .Values }}' > "$compute_yaml"
target="templates/$(basename "$compute_yaml")"
helm template release-name "$chartd" --show-only "$target" "$@"
