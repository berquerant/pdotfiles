#!/bin/bash

set -e

. "${DOTFILES_ROOT}/bin/common.sh"

ensure_cd "$PJTMP"

read -p "GIT_USER_NAME? > " gun
if [ -z "$gun" ]
then
    echo please specify GIT_USER_NAME >&2
    exit 1
fi
read -p "GIT_USER_EMAIL? > " gue
if [ -z "$gue" ]
then
    echo please specify GIT_USER_EMAIL >&2
    exit 1
fi

echo "git user.name = $gun" >&2
echo "git user.email = $gue" >&2
exec_query "generate $tmp/.gitconfig, are you sure?" "bye!"
export GIT_USER_NAME=$gun
export GIT_USER_EMAIL=$gue
envsubst < "${PROJECT}/bin/.gitconfig.tpl" > "${PJTMP}/.gitconfig"
cecho green "${PJTMP}/.gitconfig generated!"
