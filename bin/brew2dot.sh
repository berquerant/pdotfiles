#!/bin/bash

case "$1" in
    j | json2dot)
        shift
        brew deps --installed --graph --dot | rg '^ ' | tr -d " " | sed 's|->| |' | tr -d '"' | "${DOTFILES_ROOT}/bin/line2json2dot.sh" | json2dot | "${DOTFILES_ROOT}/bin/dot.sh" "$@"
        ;;
    *)
        brew deps --installed --graph --dot | "${DOTFILES_ROOT}/bin/dot.sh" "$@"
esac
