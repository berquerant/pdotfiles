#!/bin/bash

d=$(cd $(dirname $0)/..; pwd)
. $d/bin/common.sh

message() {
    cecho green "$1"
}

is_dry=false
if [ -n "$1" ]
then
    is_dry=true
    message "Do dry run"
fi

exec_or_dry() {
    run_or_dry "$1" ${is_dry}
}

message "Make symbolic links to this repository root on home directory"

exec_query "Are you sure?" "Bye!"

exec_or_dry "ln -snvf ${PROJECT} ~/"

dotfiles=(
    .zshrc
    .pythonrc.py
    .tmux.conf
    golangci.yml
    .aspell.conf
    .rustfmt.toml
)

message "Install dotfiles on home directory"

for df in "${dotfiles[@]}"
do
    exec_or_dry "ln -snvf ${d}/${df} ~/"
done

message "Install emacs configurations"

exec_or_dry "mkdir -p ${EMACSD}"
exec_or_dry "ln -snvf $d/.emacs.d/init.el ${EMACSD}/"
exec_or_dry "mkdir -p ${EMACSD}/straight/versions"
exec_or_dry "ln -snvf $d/.emacs.d/site-lisp ${EMACSD}/site-lisp"
exec_or_dry "mkdir -p ${EMACSD}/snippets"
exec_or_dry "mkdir -p ${EMACSD}/junk"
exec_or_dry "touch ${EMACSD}/abbrev_defs"
exec_or_dry "mkdir -p ${EMACSD}/external-site-lisp"

message "Install others"

exec_or_dry "ln -snvf $d/etc/peco ~/.config/"

message "Dotfiles installed!"
message "Please make gitconfig to install .gitconfig"
if ${is_dry}
then
    message "Dry run done."
fi
