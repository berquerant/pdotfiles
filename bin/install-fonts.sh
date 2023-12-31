#!/bin/bash

set -e

cd "$PJTMP"
version="v3.000"
rm -rf fonts_ttf.zip fonts
wget "https://github.com/googlefonts/Inconsolata/releases/download/${version}/fonts_ttf.zip"
unzip fonts_ttf.zip
fontfile="fonts/ttf/Inconsolata-Regular.ttf"
sudo cp fonts/ttf/Inconsolata-Regular.ttf /Library/Fonts
