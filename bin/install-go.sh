#!/bin/bash

. "${DOTFILES_ROOT}/bin/common.sh"
. "${DOTFILES_ROOT}/bin/install-git-runner.sh"

export IG_WORKD="$PJTMP"

readonly goenv_reponame="goenv"
readonly goenv_repod="${IG_WORKD}/${goenv_reponame}"
readonly goenv_location="${HOME}/.goenv"

setup_goenv() {
    ln -snvf "$goenv_repod" "$goenv_location" &&\
        goenv install "$GO_VERSION" &&\
        goenv local "$GO_VERSION"
}

rollback_goenv() {
    :
}

install_goenv() {
    :
}

ig_run "https://github.com/syndbg/goenv.git" \
       "$goenv_reponame" \
       "master" \
       "setup_goenv" \
       "install_goenv" \
       "rollback_goenv"
