#!/bin/bash

set -xe

docker version

readonly goos="darwin"
readonly goarch="amd64"
readonly docker_go_image="golang:${GO_VERSION}"
readonly repo="https://github.com/berquerant/install-via-git-go.git"
readonly reponame="install-via-git-go"
readonly repod="${PJTMP}/${reponame}"
readonly location="/usr/local/bin/install-via-git"

if [ ! -d "$repod" ] ; then
    git clone "$repo" "$repod"
fi
cd "$repod"
git pull --prune --force origin main
docker run --rm -v "$repod":/usr/src/app -w /usr/src/app -e "GOOS=${goos}" -e "GOARCH=${goarch}" "$docker_go_image" go build -v -o dist/install-via-git
ln -snvf "${repod}/dist/install-via-git" "$location"
