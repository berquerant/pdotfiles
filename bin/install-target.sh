#!/bin/bash

d="$(cd "$(dirname "$0")"/.. || exit; pwd)"
ivg_sh="${d}/bin/install-via-git.sh"
req_sh="${d}/bin/install-requirements.sh"

target="$1"
shift
set -ex

"$ivg_sh" "$target" "$@"
"$req_sh" "$target"
