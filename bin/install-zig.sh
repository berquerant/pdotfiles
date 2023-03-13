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

skipped_zig() {
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

export IVG_REPOSITORY="https://github.com/ziglang/zig.git"
export IVG_REPOSITORY_NAME="$zig_reponame"
export IVG_BRANCH="master"
export IVG_SETUP_COMMAND="setup_zig"
export IVG_INSTALL_COMMAND="install_zig"
export IVG_ROLLBACK_COMMAND="rollback_zig"
export IVG_SKIPPED_COMMAND="skipped_zig"
export IVG_LOCKFILE="${IVG_LOCKFILE_ROOT}/${zig_reponame}"
ivg_run
