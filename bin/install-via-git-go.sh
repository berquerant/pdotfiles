#!/bin/bash

set -xe

readonly goos="darwin"
readonly goarch="amd64"
readonly location="/usr/local/bin/install-via-git"
readonly ref="0.18.1"
readonly url="https://github.com/berquerant/install-via-git-go/releases/download/v${ref}/install-via-git_${ref}_${goos}_${goarch}"

if [[ -x "$location" && "$("$location" version | grep Version)" == "Version: v${ref}" ]] ; then
    exit
fi

curl -L -o "$location" "$url"
chmod +x "$location"
