#!/bin/bash

d=$(cd $(dirname $0)/..; pwd)
. $d/common.sh

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

message "Remove symbolic links to this repository root on home directory"

exec_query "Are you sure?" "Bye!"

exec_or_dry "rm -f ~/dotfiles"

dotfiles=(
    .zshrc
    .pythonrc.py
    .tmux.conf
    .tigrc
    golangci.yml
    .aspell.conf
    .gitattributes
    .gitignore
    .rustfmt.toml
)

message "Remove dotfiles on home directory"

for df in "${dotfiles[@]}"
do
    exec_or_dry "rm -f ~/${df}"
done

message "Remove emacs configurations"

exec_or_dry "rm -f ${EMACSD}/init.el"
exec_or_dry "rm -f ${EMACSD}/straight/versions/default.el"
exec_or_dry "rm -f ${EMACSD}/site-lisp"

message "Remove others"

exec_or_dry "rm -f ~/.config/peco"

message "Dotfiles removed!"
if ${is_dry}
then
    message "Dry run done."
fi
