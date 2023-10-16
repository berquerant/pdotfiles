#!/bin/bash

d=$(cd $(dirname $0); pwd)
ivgsh="${d}/install-via-git.sh"

set -e

while read target ; do
    "$ivgsh" "$target" "$@"
done
