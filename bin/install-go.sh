#!/bin/bash

. "${DOTFILES_ROOT}/bin/common.sh"
. "${DOTFILES_ROOT}/bin/install-via-git.sh"

export IVG_WORKD="$PJTMP"

readonly goenv_reponame="goenv"
readonly goenv_repod="${IVG_WORKD}/${goenv_reponame}"
readonly goenv_location="$GOENV_ROOT"

install_goenv() {
    ln -snvf "$goenv_repod" "$goenv_location" &&\
        goenv install --skip-existing "$GO_VERSION" &&\
        goenv local "$GO_VERSION"
}

ivg_run "https://github.com/syndbg/goenv.git" \
        "$goenv_reponame" \
        "master" \
        "" \
        "install_goenv"
