#!/bin/bash

d=$(cd $(dirname $0)/..; pwd)
ivgsh="${d}/bin/install-target.sh"

set -e

grep -v -E '^#' | while read target ; do
    "$ivgsh" "$target" "$@"
done
