#!/bin/bash

. "${DOTFILES_ROOT}/bin/common.sh"
. "${DOTFILES_ROOT}/bin/install-via-git.sh"

export IVG_WORKD="$PJTMP"

readonly mi_reponame="lldb-mi"
readonly mi_repod="${IVG_WORKD}/${mi_reponame}"

setup_mi() {
    brew install llvm
}

install_mi() {
    local LLVM_PATH="$(brew --prefix llvm)"
    export LDFLAGS="-L${LLVM_PATH}/lib"
    export CPPFLAGS="-I${LLVM_PATH}/include"
    export PATH="${LLVM_PATH}/bin:${PATH}"
    cd "$mi_repod" &&\
        cmake . &&\
        cmake --build . &&\
        ln -snvf "${mi_repod}/src/lldb-mi" /usr/local/bin/lldb-mi &&\
        lldb-mi --version
}

export IVG_REPOSITORY="https://github.com/lldb-tools/lldb-mi.git"
export IVG_REPOSITORY_NAME="$mi_reponame"
export IVG_SETUP_COMMAND="setup_mi"
export IVG_INSTALL_COMMAND="install_mi"
export IVG_LOCKFILE="${IVG_LOCKFILE_ROOT}/${mi_reponame}"

ivg_run
