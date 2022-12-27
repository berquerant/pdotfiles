#!/bin/bash

. "${DOTFILES_ROOT}/bin/common.sh"
. "${DOTFILES_ROOT}/bin/install-git-runner.sh"

export IG_WORKD="$PJTMP"

readonly pyenv_reponame="pyenv"
readonly pyenv_repod="${IG_WORKD}/${pyenv_reponame}"
readonly pyenv_location="${HOME}/.pyenv"

setup_pyenv() {
    ln -snvf "$pyenv_repod" "$pyenv_location" &&\
        pyenv install "$PY_VERSION" &&\
        pyenv local "$PY_VERSION"
}

rollback_pyenv() {
    :
}

install_pyenv() {
    :
}

ig_run "https://github.com/pyenv/pyenv.git" \
       "$pyenv_reponame" \
       "master" \
       "setup_pyenv" \
       "install_pyenv" \
       "rollback_pyenv"
