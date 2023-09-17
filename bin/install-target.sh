#!/bin/bash

d=$(cd $(dirname $0)/..; pwd)
ivg_sh="${d}/bin/install-via-git.sh"
req_sh="${d}/bin/install-requirements.sh"

target="$1"
update="$2"

set -ex

if [ -z "$update" ] ; then
    "$ivg_sh" "$target"
else
    "$ivg_sh" "$target" --update
fi

"$req_sh" "$target"
