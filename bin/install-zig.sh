#!/bin/bash

. "${DOTFILES_ROOT}/bin/common.sh"
. "${DOTFILES_ROOT}/bin/install-via-git.sh"

export IVG_WORKD="$PJTMP"

readonly zig_reponame="zig"
readonly zig_repod="${IVG_WORKD}/${zig_reponame}"
readonly zig_location="${zig_repod}/build/stage3/bin/zig"
readonly zig_location_backup="${zig_repod}/build/stage3/bin/zig.bk"

backup_zig() {
    rm -rf "$zig_location_backup"
    mv "$zig_location" "$zig_location_backup"
}

restore_zig() {
    mv "$zig_location_backup" "$zig_location"
}

rollback_zig() {
    restore_zig
}

setup_zig() {
    backup_zig || brew install llvm zstd
}

install_zig() {
    cd "$zig_repod" &&\
        mkdir -p build &&\
        cd build &&\
        cmake .. -DZIG_STATIC_LLVM=on -DZIG_STATIC_ZLIB=on -DZIG_PREFER_CLANG_CPP_DYLIB=on -DCMAKE_PREFIX_PATH="$(brew --prefix llvm);$(brew --prefix zstd)" &&\
        make install &&\
        ln -snvf "$zig_location" /usr/local/bin/zig &&\
        zig version
}

prepare_zig() {
    ivg_run "https://github.com/ziglang/zig.git" \
            "$zig_reponame" \
            "master" \
            "setup_zig" \
            "install_zig" \
            "rollback_zig"
}

readonly zls_reponame="zls"
readonly zls_repod="${IVG_WORKD}/${zls_reponame}"
readonly zls_location="${zls_repod}/zig-out/bin/zls"
readonly zls_location_backup="${zls_location}.bk"

backup_zls() {
    rm -rf "$zls_location_backup"
    mv "$zls_location" "$zls_location_backup"
}

restore_zls() {
    mv "$zls_location_backup" "$zls_location"
}

setup_zls() {
    backup_zls || return 0
}

rollback_zls() {
    restore_zls
}

install_zls() {
    cd "$zls_repod" &&\
        git submodule update --init &&\
        zig build -Drelease-safe &&\
        ln -snvf "$zls_location" /usr/local/bin/zls &&\
        zls --version
}

prepare_zls() {
    ivg_run "https://github.com/zigtools/zls" \
            "$zls_reponame" \
            "master" \
            "setup_zls" \
            "install_zls" \
            "rollback_zls"
}

prepare_zig && prepare_zls
