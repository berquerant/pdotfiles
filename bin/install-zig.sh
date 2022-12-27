#!/bin/bash

. "${DOTFILES_ROOT}/bin/common.sh"
. "${DOTFILES_ROOT}/bin/install-git-runner.sh"

export IG_WORKD="$PJTMP"

readonly zig_reponame="zig"
readonly zig_repod="${IG_WORKD}/${zig_reponame}"

setup_zig() {
    brew install llvm zstd
}

install_zig() {
    cd "$zig_repod" &&\
        mkdir build &&\
        cd build &&\
        cmake .. -DZIG_STATIC_LLVM=ON -DCMAKE_PREFIX_PATH="$(brew --prefix llvm);$(brew --prefix zstd)" &&\
        make install &&\
        ln -snvf "${zig_repod}/build/stage3/bin/zig" /usr/local/bin/zig &&\
        zig version
}

rollback_zig() {
    :
}

prepare_zig() {
    ig_run "https://github.com/ziglang/zig.git" \
           "$zig_reponame" \
           "master" \
           "setup_zig" \
           "install_zig" \
           "rollback_zig"
}

readonly zls_reponame="zls"
readonly zls_repod="${IG_WORKD}/${zls_reponame}"

setup_zls() {
    cd "$zls_repod" && git submodule update --init
}

install_zls() {
    cd "$zls_repod" &&\
        zig build -Drelease-safe &&\
        ln -snvf "${zls_repod}/zig-out/bin/zls" /usr/local/bin/zls &&\
        zls --version
}

rollback_zls() {
    cecho red "zls rollback noop"
}

prepare_zls() {
    ig_run "https://github.com/zigtools/zls" \
           "$zls_reponame" \
           "master" \
           "setup_zls" \
           "install_zls" \
           "rollback_zls"
}

prepare_zig && prepare_zls
