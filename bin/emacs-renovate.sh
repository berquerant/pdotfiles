#!/bin/bash

python -m straight.cli \
       -e "${EMACSD}/straight-default.el" \
       -d "${EMACSD}/straight/repos" \
       -r "${DOTFILES_ROOT}/.emacs.d/renovate.lock" \
       "$@"
