#!/bin/bash

ivg="install-via-git"

if [ ! -e $(which $ivg) ] ; then
    echo "Please make install-via-git to install it"
    exit 1
fi

target="$1"
if [ -z "$target" ] ; then
    echo "target is required"
    exit 1
fi
shift

ivg_configd="${DOTFILES_ROOT}/ivg"
ivg_workd="${DOTFILES_ROOT}/ivg"
config="${ivg_configd}/${target}.yml"

set -x
"$ivg" run --workDir "$ivg_workd" --config "$config" "$@"
