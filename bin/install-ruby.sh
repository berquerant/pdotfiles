#!/bin/bash

. "${DOTFILES_ROOT}/bin/common.sh"
. "${DOTFILES_ROOT}/bin/install-via-git.sh"

export IVG_WORKD="$PJTMP"

readonly rbenv_reponame="rbenv"
readonly rbenv_repod="${IVG_WORKD}/${rbenv_reponame}"
readonly rbenv_location="$RBENV_ROOT"

setup_rbenv() {
    brew install openssl@3 readline libyaml gmp ruby-build
}

install_rbenv() {
    ln -snvf "$rbenv_repod" "$rbenv_location" &&\
        eval "$(${RBENV_ROOT}/bin/rbenv init - zsh)" &&\
        RUBY_CONFIGURE_OPTS="--with-openssl-dir=$(brew --prefix openssl@3) rb_cv_symbol_prefix=_" rbenv install "$RB_VERSION" &&\
        rbenv local "$RB_VERSION" &&\
        gem install bundler solargraph rubocop
}

export IVG_REPOSITORY="https://github.com/rbenv/rbenv"
export IVG_REPOSITORY_NAME="$rbenv_reponame"
export IVG_SETUP_COMMAND="setup_rbenv"
export IVG_INSTALL_COMMAND="install_rbenv"
export IVG_BRANCH="master"
export IVG_LOCKFILE="${IVG_LOCKFILE_ROOT}/${rbenv_reponame}"

ivg_run
