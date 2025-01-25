#!/bin/bash

d=$(cd "$(dirname "$0")" || exit; pwd)
bin="${d}/tmp/xc"
ref=0.8.5

exist_matched_binary() {
    [ -x "$bin" ] && [ "$("$bin" -V | cut -d ' ' -f 3)" = "$ref" ]
}

if ! exist_matched_binary ; then
    mkdir -p "$(dirname "$bin")"
    curl -L -o "$bin" "https://github.com/joerdav/xc/releases/download/v${ref}/xc_${ref}_darwin_arm64"
    chmod +x "$bin"
fi

"$bin" "$@"
