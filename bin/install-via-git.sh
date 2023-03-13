#!/bin/bash

ivg_sh="${DOTFILES_ROOT}/install-via-git-sh/install-via-git.sh"

if [ ! -f "$ivg_sh" ]; then
    echo "Please make init to enable install-via-git-sh"
    return 1
fi

export IVG_LOCKFILE_ROOT="${DOTFILES_ROOT}/.ivg.lock"
. "$ivg_sh"
