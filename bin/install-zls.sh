#!/bin/bash

. "${DOTFILES_ROOT}/bin/common.sh"
. "${DOTFILES_ROOT}/bin/install-via-git.sh"

export IVG_WORKD="$PJTMP"

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

skipped_zls() {
    restore_zls
}

install_zls() {
    cd "$zls_repod" &&\
        git submodule update --init &&\
        zig build -Drelease-safe &&\
        ln -snvf "$zls_location" /usr/local/bin/zls &&\
        zls --version
}

export IVG_REPOSITORY="https://github.com/zigtools/zls"
export IVG_REPOSITORY_NAME="$zls_reponame"
export IVG_BRANCH="master"
export IVG_SETUP_COMMAND="setup_zls"
export IVG_INSTALL_COMMAND="install_zls"
export IVG_ROLLBACK_COMMAND="rollback_zls"
export IVG_SKIPPED_COMMAND="skipped_zls"
export IVG_LOCKFILE="${IVG_LOCKFILE_ROOT}/${zls_reponame}"
ivg_run
