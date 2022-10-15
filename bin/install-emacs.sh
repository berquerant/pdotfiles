#!/bin/bash

source ${DOTFILES_ROOT}/bin/common.sh

cecho green "Install GCC Emacs."
cecho green "This takes a long time..."

brew install --cask xquartz
brew install --build-from-source libgccjit
brew install cmigemo

cecho green "Backing up Emacs."

rm -rf /Applications/Emacs.app.bk
mv /Applications/Emacs.app /Applications/Emacs.app.bk
rm -rf ${EMACSD}/straight.bk
mv ${EMACSD}/straight ${EMACSD}/straight.bk

mkdir -p ${PROJECT}/tmp
cd ${PROJECT}/tmp

if [ ! -d ./emacs ]; then
    cecho green "Download emacs repo."
    git clone git://git.sv.gnu.org/emacs.git
fi

cd ./emacs
CURRENT_HASH=$(git rev-parse HEAD)
cecho green "Now emacs is ${CURRENT_HASH}"
git pull
NEXT_HASH=$(git rev-parse HEAD)
cecho green "Next emacs will be ${NEXT_HASH}"

rollback() {
    mv /Applications/Emacs.app.bk /Applications/Emacs.app
    mv ${EMACSD}/straight.bk ${EMACSD}/straight
    cecho yellow "Emacs rolled back!"
}

on_failed() {
    cecho red "Install GCC Emacs ${NEXT_HASH} failed!"
    rollback
    cecho red "Now emacs is ${CURRENT_HASH}"
    exit 1
}

on_success() {
    mv nextstep/Emacs.app /Applications/
    make clean
    cecho green "GCC Emacs Installed!"
    cecho green "Now emacs is ${NEXT_HASH}"
    cecho green "Done!"
}

find . -name "*.pdmp" -type f -delete
rm -rf ${EMACSD}/eln-cache

# emacs at commit d5e74d9cd7
#
# ./configure causes an error:
# configure: error: ELisp native compiler was requested, but libgccjit was not found.
#
# is caused by MAC_LIBS, that contains multiple lines.
#
# apply patch:
# --- a/configure.ac
# +++ b/configure.ac
# @@ -4241,7 +4241,7 @@ AC_DEFUN
#            MAC_CFLAGS="-I$(dirname $($BREW ls -v libgccjit | \
#                                                  grep libgccjit.h))"
#            MAC_LIBS="-L$(dirname $($BREW ls -v libgccjit| \
# -                                            grep -E 'libgccjit\.(so|dylib)$'))"
# +                                            grep -E 'libgccjit\.(so|dylib)$'|grep 'current'))"
#          fi
#        fi
export CC=clang
./autogen.sh &&\
  ./configure AR="/usr/bin/ar" RANLIB="/usr/bin/ranlib" CFLAGS="-O3" --with-native-compilation --with-xwidgets --without-webp --without-sound &&\
  make -j4 && make install &&\
  on_success || on_failed
