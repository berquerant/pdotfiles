#!/bin/bash

. "${DOTFILES_ROOT}/bin/common.sh"
. "${DOTFILES_ROOT}/bin/install-via-git.sh"

export IVG_WORKD="$PJTMP"

readonly pyenv_reponame="pyenv"
readonly pyenv_repod="${IVG_WORKD}/${pyenv_reponame}"
readonly pyenv_location="$PYENV_ROOT"

install_pyenv() {
    # FIXME:
    # want: ~/.pyenv -> $PJTMP/pyenv
    # now: ~/.pyenv/pyenv -> $PJTMP/pyenv
    ln -snvf "$pyenv_repod" "$pyenv_location" &&\
        pyenv install --skip-existing "$PY_VERSION" &&\
        pyenv local "$PY_VERSION"
}

export IVG_REPOSITORY="https://github.com/pyenv/pyenv.git"
export IVG_REPOSITORY_NAME="$pyenv_reponame"
export IVG_BRANCH="master"
export IVG_INSTALL_COMMAND="install_pyenv"
export IVG_LOCKFILE="${IVG_LOCKFILE_ROOT}/${pyenv_reponame}"

ivg_run
