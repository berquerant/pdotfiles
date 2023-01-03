#!/bin/bash

if [[ ! -d "${PJTMP}/install-via-git-sh" ]]; then
    git clone "https://github.com/berquerant/install-via-git-sh.git" "${PJTMP}/install-via-git-sh"
fi

. "${PJTMP}/install-via-git-sh/install-via-git.sh"
