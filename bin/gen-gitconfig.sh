#!/bin/bash

d=$(cd $(dirname $0); pwd)
. ${d}/common.sh

tmp=${d}/../tmp
mkdir -p $tmp

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
envsubst < ${d}/.gitconfig.tpl > ${tmp}/.gitconfig
cecho green "${tmp}/.gitconfig generated!"
