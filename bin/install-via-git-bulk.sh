#!/bin/bash

d="$(cd "$(dirname "$0")"/.. || exit; pwd)"
ivgsh="${d}/bin/install-via-git.sh"

set -e

grep -v -E '^#' | while read -r target ; do
    "$ivgsh" "$target" "$@"
done
