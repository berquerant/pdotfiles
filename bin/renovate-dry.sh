#!/bin/bash

readonly config="${CONFIG:-renovate.json}"
# shellcheck disable=SC2153
readonly token="$TOKEN"

target_repopath() {
    git config --get remote.origin.url |\
        tr ":" "/" |\
        sed -E 's|git@|https///|' |\
        sed -E 's|git///|https///|' |\
        sed -E 's|\.git||' |\
        sed -E 's|https///|https://|' |\
        sed -E 's|https://||' |\
        cut -d '/' -f 2-
}

run() {
    docker run --rm -it \
           -e LOG_LEVEL=debug \
           -e RENOVATE_CONFIG_FILE="$config" \
           -v "$PWD:/app/src" \
           -w "/app/src" \
           renovate/renovate \
           renovate \
           --dry-run=full \
           --schedule= \
           --require-config=ignored \
           --token "$token" \
           "$(target_repopath)" \
           "$@"
}

run "$@"
