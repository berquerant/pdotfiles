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
        nvm install --lts --latest-npm &&\
        npm i -g eslint csslint typescript typescript-formatter typescript-eslint typescript-language-server ts-node
}

ivg_run "https://github.com/nvm-sh/nvm.git" \
        "$nvm_reponame" \
        "master" \
        "" \
        "install_nvm"
