#!/bin/bash

set -xe

docker version

readonly repo="https://github.com/berquerant/install-via-git-go.git"
readonly reponame="install-via-git-go"
readonly repod="${PJTMP}/${reponame}"
readonly location="/usr/local/bin/install-via-git"

if [ ! -d "$repod" ] ; then
    git clone "$repo" "$repod"
fi
cd "$repod"
git pull --prune --force origin main
docker run --rm -v "$repod":/usr/src/app -w /usr/src/app -e GOOS=darwin -e GOARCH=amd64 golang:1.21.1 go build -v -o dist/install-via-git
ln -snvf "${repod}/dist/install-via-git" "$location"
