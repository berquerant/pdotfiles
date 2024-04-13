#!/bin/bash

init() {
    set -ex
    ghq_root="${GHQ_ROOT}/"
    repo_root="${PWD#$ghq_root}"
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
