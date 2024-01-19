#!/bin/bash

set -e

. "${DOTFILES_ROOT}/bin/common.sh"

ensure_cd "$PJTMP"

if [ -z "$GIT_USER_NAME" ] ; then
    read -p "GIT_USER_NAME? > " gun
    if [ -z "$gun" ]
    then
        echo GIT_USER_NAME >&2
        exit 1
    fi
    export GIT_USER_NAME=$gun
fi
if [ -z "$GIT_USER_EMAIL" ] ; then
    read -p "GIT_USER_EMAIL? > " gue
    if [ -z "$gue" ]
    then
        echo GIT_USER_EMAIL >&2
        exit 1
    fi
    export GIT_USER_EMAIL=$gue
fi

echo "git user.name = ${GIT_USER_NAME}" >&2
echo "git user.email = ${GIT_USER_EMAIL}" >&2

current_config="${HOME}/.gitconfig"
next_config="${PJTMP}/.gitconfig"
envsubst < "${PROJECT}/bin/.gitconfig.tpl" > "$next_config"

set +e
if [ -f "$current_config" ]; then
    if diff -u "$current_config" "$next_config" ; then
        cecho green "no changes"
    fi
fi
cecho green "${next_config} generated!"
cp -i "$next_config" "$current_config"

cecho green "check git aliases..."
if git aliases ; then
    cecho green "OK!"
else
    cecho red "git aliases failed! check ${current_config}"
    exit 1
fi
