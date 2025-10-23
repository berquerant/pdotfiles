#!/bin/bash

readonly d="$(cd "$(dirname "$0")"/.. || exit 1; pwd)"

fzf() {
    "${d}/bin/fzf.sh"
}

resources() {
    kubectl api-resources --no-headers -o json |\
        jq -r '
.resources[] |
(.name) + "\t" + (if .group != null then ((.group)+"/"+(.version)) else (.version) end)'
}

infer_endpoint() {
    local -r api_group="$1"
    if [[ "$api_group" = "v1" ]] ; then
        # Core V1
        echo "/api/v1"
    else
        echo "/apis/${api_group}"
    fi
}

getraw() {
    kubectl get --raw "$@"
}


set -e
set -o pipefail

resources | fzf | awk '{print $2}' | while read -r x ; do
    e="$(infer_endpoint "$x")"
    getraw "$e"
done
