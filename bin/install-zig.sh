#!/bin/bash

. "${DOTFILES_ROOT}/bin/common.sh"
. "${DOTFILES_ROOT}/bin/install-via-git.sh"

export IVG_WORKD="$PJTMP"

readonly zig_reponame="zig"
readonly zig_repod="${IVG_WORKD}/${zig_reponame}"

setup_zig() {
    brew install llvm zstd
}

install_zig() {
    cd "$zig_repod" &&\
        mkdir -p build &&\
        cd build &&\
        cmake .. -DZIG_STATIC_LLVM=on -DZIG_STATIC_ZLIB=on -DZIG_PREFER_CLANG_CPP_DYLIB=on -DCMAKE_PREFIX_PATH="$(brew --prefix llvm);$(brew --prefix zstd)" &&\
        make install &&\
        ln -snvf "${zig_repod}/build/stage3/bin/zig" /usr/local/bin/zig &&\
        zig version
}

prepare_zig() {
    ivg_run "https://github.com/ziglang/zig.git" \
            "$zig_reponame" \
            "master" \
            "setup_zig" \
            "install_zig"
}

readonly zls_reponame="zls"
readonly zls_repod="${IVG_WORKD}/${zls_reponame}"

setup_zls() {
    cd "$zls_repod" && git submodule update --init
}

install_zls() {
    cd "$zls_repod" &&\
        zig build -Drelease-safe &&\
        ln -snvf "${zls_repod}/zig-out/bin/zls" /usr/local/bin/zls &&\
        zls --version
}

prepare_zls() {
    ivg_run "https://github.com/zigtools/zls" \
            "$zls_reponame" \
            "master" \
            "setup_zls" \
            "install_zls"
}

prepare_zig && prepare_zls
