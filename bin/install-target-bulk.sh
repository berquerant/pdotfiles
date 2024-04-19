#!/bin/bash

d=$(cd $(dirname $0)/..; pwd)
ivgsh="${d}/bin/install-target.sh"

set -e

while read target ; do
    "$ivgsh" "$target" "$@"
done
