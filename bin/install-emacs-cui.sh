#!/bin/bash

. "${DOTFILES_ROOT}/bin/common.sh"
. "${DOTFILES_ROOT}/bin/install-via-git.sh"

export IVG_WORKD="$PJTMP"

readonly emacs_reponame="emacs-cui"
readonly emacs_repod="${IVG_WORKD}/${emacs_reponame}"

readonly emacs_compile_concurrency="${EMACS_COMPILE_CONCURRENCY:-4}"

setup_emacs_brew() {
    brew install --cask xquartz &&\
        brew install gcc flann cmigemo gnutls texinfo &&\
        brew install --build-from-source libgccjit
}

readonly emacs_location="/Applications/Emacs-CUI.app"
readonly emacs_location_backup="${emacs_location}.bk"

backup_emacs() {
    rm -rf "$emacs_location_backup"
    mv "$emacs_location" "$emacs_location_backup"
}

restore_emacs() {
    mv "$emacs_location_backup" "$emacs_location"
}

rollback_emacs() {
    restore_emacs
}

setup_emacs() {
    backup_emacs || setup_emacs_brew || return $?
    if ! grep --quiet "/usr/local/opt/texinfo/bin" ~/.zprofile ; then
        echo 'export PATH=/usr/local/opt/texinfo/bin:$PATH' >> ~/.zprofile
    fi
}

skipped_emacs() {
    restore_emacs
}

install_emacs() {
    export PATH=/usr/local/opt/texinfo/bin:$PATH
    export CC=clang
    cd "$emacs_repod" &&\
        find . -name "*.pdmp" -type f -delete &&\
        find . -name "*.elc" -type f -delete &&\
        rm -rf "${EMACSD}/eln-cache" &&\
        make distclean &&\
        ./autogen.sh &&\
        ./configure AR="/usr/bin/ar" RANLIB="/usr/bin/ranlib" \
                    --with-native-compilation \
                    --with-ns \
                    --without-webp \
                    --without-sound \
                    --without-pop \
                    --without-x \
                    --without-dbus \
                    --without-gconf \
                    --without-gsettings \
                    --without-xwidgets \
                    --without-xaw3d \
                    --without-gpm \
                    --without-makeinfo \
                    --without-mailutils &&\
        make -j"$emacs_compile_concurrency" &&\
        make install &&\
        mv nextstep/Emacs.app "$emacs_location" &&\
        make distclean
}

export IVG_REPOSITORY="git://git.sv.gnu.org/emacs.git"
export IVG_REPOSITORY_NAME="$emacs_reponame"
export IVG_BRANCH="master"
export IVG_SETUP_COMMAND="setup_emacs"
export IVG_INSTALL_COMMAND="install_emacs"
export IVG_ROLLBACK_COMMAND="rollback_emacs"
export IVG_SKIPPED_COMMAND="skipped_emacs"
export IVG_LOCKFILE="${IVG_LOCKFILE_ROOT}/${emacs_reponame}"

ivg_run
