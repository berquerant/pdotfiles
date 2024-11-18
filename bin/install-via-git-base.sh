#!/bin/bash

d="$(cd "$(dirname "$0")"/.. || exit; pwd)"
. "${d}/bin/common.sh"
ivg="install-via-git"

if [ ! -e "$(which $ivg)" ] ; then
    "${d}/bin/install-via-git-go.sh" || exit 1
fi

cmd="$1"
target="$2"
if [ -z "$cmd" ] ; then
    cecho red "cmd is required"
    cecho green "available cmds: run, uninstall"
    exit 1
fi
if [ -z "$target" ] ; then
    cecho red "target is required"
    cecho green "available targets:"
    git ls-files | grep 'ivg/.*.yml$' | xargs -n 1 basename | cut -d "." -f 1
    exit 1
fi

shift 2

ivg_configd="${DOTFILES_ROOT}/ivg"
ivg_workd="${DOTFILES_ROOT}/ivg"
config="${ivg_configd}/${target}.yml"

set -x
"$ivg" "$cmd" --workDir "$ivg_workd" --config "$config" "$@"
