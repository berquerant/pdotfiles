#!/bin/bash

d=$(cd $(dirname $0)/..; pwd)
. "${d}/bin/common.sh"
ivg="install-via-git"

if [ ! -e "$(which $ivg)" ] ; then
    "${d}/bin/install-via-git-go.sh" || exit 1
fi

target="$1"
if [ -z "$target" ] ; then
    echo "target is required"
    exit 1
fi

if [ -n "$IVG_SH_IGNORE" ] ; then
    if echo "$target" | grep -q -E "$IVG_SH_IGNORE" ; then
        cecho yellow "$(basename $0) ignores ${target} because IVG_SH_IGNORE=${IVG_SH_IGNORE} matched"
        exit
    fi
fi

shift

ivg_configd="${DOTFILES_ROOT}/ivg"
ivg_workd="${DOTFILES_ROOT}/ivg"
config="${ivg_configd}/${target}.yml"

set -x
"$ivg" run --workDir "$ivg_workd" --config "$config" "$@"
