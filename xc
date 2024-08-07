#!/bin/bash

d=$(cd $(dirname $0); pwd)
bin="${d}/tmp/xc"

if [ ! -x "$bin" ] ; then
    mkdir -p "$(dirname $bin)"
    curl -L -o "$bin" https://github.com/joerdav/xc/releases/download/v0.8.0/xc_0.8.0_darwin_arm64
    chmod +x "$bin"
fi
"$bin" "$@"
