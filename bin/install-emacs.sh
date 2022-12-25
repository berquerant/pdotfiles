#!/bin/bash

. "${DOTFILES_ROOT}/bin/common.sh"
. "${DOTFILES_ROOT}/bin/install-git-runner.sh"

export IG_WORKD="$PJTMP"

readonly emacs_reponame="emacs"
readonly emacs_repod="${IG_WORKD}/${emacs_reponame}"

setup_emacs_brew() {
    brew install --cask xquartz &&\
        brew install --build-from-source libgccjit &&\
        brew install cmigemo gnutls texinfo
}

readonly emacs_location="/Applications/Emacs.app"
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
    backup_emacs && setup_emacs_brew || return $?
    if ! grep --quiet "/usr/local/opt/texinfo/bin" ~/.zprofile ; then
        echo 'export PATH=/usr/local/opt/texinfo/bin:$PATH' >> ~/.zprofile
    fi
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
        ./configure AR="/usr/bin/ar" RANLIB="/usr/bin/ranlib" --with-native-compilation --with-xwidgets --without-webp --without-sound --without-pop &&\
        make -j4 &&\
        make install &&\
        mv nextstep/Emacs.app /Applications/ &&\
        make distclean
}

ig_run "git://git.sv.gnu.org/emacs.git" \
       "$emacs_reponame" \
       "master" \
       "setup_emacs" \
       "install_emacs" \
       "rollback_emacs"
