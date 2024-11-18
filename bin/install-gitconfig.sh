#!/bin/bash

set -e

. "${DOTFILES_ROOT}/bin/common.sh"

ensure_cd "$PJTMP"

GIT_USER_NAME="$(git config user.name)"
GIT_USER_EMAIL="$(git config user.email)"
export GIT_USER_NAME
export GIT_USER_EMAIL

if [ -z "$GIT_USER_NAME" ] ; then
    read -r -p "GIT_USER_NAME? > " gun
    if [ -z "$gun" ]
    then
        cecho red "No GIT_USER_NAME!" >&2
        exit 1
    fi
    export GIT_USER_NAME=$gun
fi
if [ -z "$GIT_USER_EMAIL" ] ; then
    read -r -p "GIT_USER_EMAIL? > " gue
    if [ -z "$gue" ]
    then
        cecho red "No GIT_USER_EMAIL!" >&2
        exit 1
    fi
    export GIT_USER_EMAIL=$gue
fi

cecho green "git user.name = ${GIT_USER_NAME}" >&2
cecho green "git user.email = ${GIT_USER_EMAIL}" >&2

current_config="${HOME}/.gitconfig"
next_config="${PJTMP}/.gitconfig"
backup_config="${PJTMP}/.gitconfig.backup"
envsubst < "${PROJECT}/.gitconfig.tpl" > "$next_config"
if [ -f "$current_config" ] ; then
    config_backup_exist=1
    cp "$current_config" "$backup_config"
fi

cecho green "${next_config} generated!"
set +e
if [ -n "$GIT_CONFIG_DRYRUN" ] ; then
    cecho yellow "No files were copied due to dry run"
    exit
fi

if [ -n "$config_backup_exist" ] ; then
    if diff -u "$current_config" "$next_config" ; then
        cecho green "No changes"
        exit
    fi
fi

cp "$next_config" "$current_config"

cecho green "check git aliases..."
if git aliases ; then
    cecho green "OK!"
    cecho green "Backup: ${backup_config}"
else
    cecho red "git aliases failed! check ${current_config}"
    cecho red "Backup: ${backup_config}"
    exit 1
fi
