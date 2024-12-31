#!/bin/bash

d="$(cd "$(dirname "$0")"/.. || exit; pwd)"
. "${d}/bin/common.sh"

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

message "Install emacs configurations"

exec_or_dry "mkdir -p ${EMACSD}"
exec_or_dry "ln -snvf $d/.emacs.d/init.el ${EMACSD}/"
exec_or_dry "ln -snvf $d/.emacs.d/straight-default.el ${EMACSD}/straight-default.el"
exec_or_dry "ln -snvf $d/.emacs.d/site-lisp ${EMACSD}/site-lisp"
exec_or_dry "mkdir -p ${EMACSD}/snippets"
exec_or_dry "mkdir -p ${EMACSD}/junk"
exec_or_dry "touch ${EMACSD}/abbrev_defs"
exec_or_dry "mkdir -p ${EMACSD}/external-site-lisp"
