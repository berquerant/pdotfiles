#!/bin/bash

d=$(cd "$(dirname "$0")" || exit; pwd)
bin="${d}/tmp/xc"
ref=0.8.6

exist_matched_binary() {
    [ -x "$bin" ] && [ "$("$bin" -V | cut -d ' ' -f 3)" = "$ref" ]
}

download_binary() {
    set -ex
    local -r bin_dir="$(dirname "$bin")"
    mkdir -p "${bin_dir}"
    curl -s -L -o "${bin_dir}/checksums.txt" "https://github.com/joerdav/xc/releases/download/v${ref}/checksums.txt"
    local os="darwin"
    local arch="arm64"
    # https://docs.github.com/en/actions/writing-workflows/choosing-what-your-workflow-does/store-information-in-variables#default-environment-variables
    if [ -n "$CI" ] ; then
        os="linux"
        arch="amd64"
    fi
    curl -s -L -o "$bin" "https://github.com/joerdav/xc/releases/download/v${ref}/xc_${ref}_${os}_${arch}"
    grep "$(sha256sum "$bin" | cut -d ' ' -f 1)" "${bin_dir}/checksums.txt"
    chmod +x "$bin"
    set +ex
}

if ! exist_matched_binary ; then
    download_binary
fi

"$bin" "$@"
