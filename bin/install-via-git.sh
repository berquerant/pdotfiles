#!/bin/bash

d="$(cd "$(dirname "$0")"/.. || exit; pwd)"
ivg="${d}/bin/install-via-git-base.sh"
target="$1"
shift
set -x
"$ivg" run "$target" "$@"
