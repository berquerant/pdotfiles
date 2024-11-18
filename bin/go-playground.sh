#!/bin/bash

init() {
    set -ex
    # shellcheck disable=SC2153
    local ghq_root="${GHQ_ROOT}/"
    local repo_root="${PWD#"$ghq_root"}"
    go mod init "$repo_root"
    go mod tidy
}

run() {
    set -ex
    go mod tidy
    go run ./snippet.go
}

case "$1" in
    init)
        init
        ;;
    run)
        run
        ;;
    *)
        echo "arg: init|run" >&2
        exit 1
        ;;
esac
