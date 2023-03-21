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
export GIT_USER_NAME=$gun
export GIT_USER_EMAIL=$gue

current_config="${HOME}/.gitconfig"
next_config="${PJTMP}/.gitconfig"
envsubst < "${PROJECT}/bin/.gitconfig.tpl" > "$next_config"

set +e
if [ -f "$current_config" ]; then
    diff "$current_config" "$next_config"
fi
cecho green "${next_config} generated!"
cp -i "$next_config" "$current_config"
