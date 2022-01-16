#!/bin/bash

source $DOTFILES_ROOT/bin/common.sh

cecho green "Install GCC Emacs."
cecho green "This takes a long time..."

brew install --cask xquartz
brew install --build-from-source libgccjit
brew install cmigemo

mkdir -p ${PROJECT}/tmp
cd ${PROJECT}/tmp || exit 1
git clone git://git.sv.gnu.org/emacs.git
cd ./emacs || exit 1
find . -name "*.pdmp" -type f -exec rm -f {} \;
export CC=clang
./autogen.sh &&
  ./configure --with-native-compilation --with-xwidgets &&\
  make -j4 && make install &&\
  mv nextstep/Emacs.app /Applications/ || exit 1
cecho green "GCC Emacs Installed!"
cecho green "Done!"
