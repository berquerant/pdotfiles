#!/bin/bash

d=$(cd $(dirname $0); pwd)
ivg="install-via-git"

if [ ! -e "$(which $ivg)" ] ; then
    "${d}/install-via-git-go.sh" || exit 1
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
