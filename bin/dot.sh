#!/bin/bash

extension="$1"
if [ -z "$extension" ] ; then
    echo "extension required"
    echo "$(basename $0) EXTENSION [DOT_OPTIONS]"
    echo "render graph by dot"
    exit 1
fi
shift

out="$(mktemp).${extension}"

dot -T"$extension" -o"$out" "$@" && open "$out"
echo "$out"
