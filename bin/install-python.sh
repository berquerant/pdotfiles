#!/bin/bash

. "${DOTFILES_ROOT}/bin/common.sh"
. "${DOTFILES_ROOT}/bin/install-via-git.sh"

export IVG_WORKD="$PJTMP"

readonly pyenv_reponame="pyenv"
readonly pyenv_repod="${IVG_WORKD}/${pyenv_reponame}"
readonly pyenv_location="$PYENV_ROOT"

install_pyenv() {
    ln -snvf "$pyenv_repod" "$pyenv_location" &&\
        pyenv install --skip-existing "$PY_VERSION" &&\
        pyenv local "$PY_VERSION"
}

ivg_run "https://github.com/pyenv/pyenv.git" \
        "$pyenv_reponame" \
        "master" \
        "" \
        "install_pyenv"
