#!/bin/bash

source $DOTFILES_ROOT/bin/common.sh

cecho green "Install GCC Emacs."
cecho green "This takes a long time..."

mkdir -p ${PROJECT}/tmp
cd ${PROJECT}/tmp
git clone git://git.sv.gnu.org/emacs.git
cd ./emacs
./autogen.sh &&\
    ./configure --with-native-compilation --with-xwidgets &&\
    make &&\
    make install &&\
    mv nextstep/Emacs.app /Applications/ &&\
    cecho green "GCC Emacs Installed!"

cecho green "Install font..."
cd ~/Library/Fonts && wget https://github.com/adobe-fonts/source-han-code-jp/releases/download/2.012R/SourceHanCodeJP.ttc
cecho green "Done!"
