#!/bin/bash

. "${DOTFILES_ROOT}/bin/common.sh"
. "${DOTFILES_ROOT}/bin/install-git-runner.sh"

export IG_WORKD="$PJTMP"

readonly mi_reponame="lldb-mi"
readonly mi_repod="${IG_WORKD}/${mi_reponame}"

setup_mi() {
    brew install llvm
}

install_mi() {
    readonly LLVM_PATH="$(brew --prefix llvm)"
    export LDFLAGS="-L${LLVM_PATH}/lib"
	  export CPPFLAGS="-I${LLVM_PATH}/include"
	  export PATH="${LLVM_PATH}/bin:${PATH}"
    cd "$mi_repod" &&\
        cmake . &&\
        cmake --build . &&\
        ln -snvf "${mi_repod}/src/lldb-mi" /usr/local/bin/lldb-mi &&\
        lldb-mi --version
}

rollback_mi() {
    :
}

ig_run "https://github.com/lldb-tools/lldb-mi.git" \
       "$mi_reponame" \
       "main" \
       "setup_mi" \
       "install_mi" \
       "rollback_mi"
