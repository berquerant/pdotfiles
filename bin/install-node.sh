#!/bin/bash

. "${DOTFILES_ROOT}/bin/common.sh"
. "${DOTFILES_ROOT}/bin/install-via-git.sh"

export IVG_WORKD="$PJTMP"

readonly nvm_reponame="nvm"
readonly nvm_repod="${IVG_WORKD}/${nvm_reponame}"
readonly nvm_location="$NVM_DIR"

install_nvm() {
    ln -snvf "$nvm_repod" "$nvm_location" &&\
        . "${NVM_DIR}/nvm.sh" &&\
        nvm install --lts --latest-npm
}

export IVG_REPOSITORY="https://github.com/nvm-sh/nvm.git"
export IVG_REPOSITORY_NAME="$nvm_reponame"
export IVG_INSTALL_COMMAND="install_nvm"
export IVG_BRANCH="master"
export IVG_LOCKFILE="${IVG_LOCKFILE_ROOT}/${nvm_reponame}"

ivg_run
