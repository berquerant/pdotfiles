#!/bin/bash

while read -r line ; do
    src="$(echo "$line" | awk '{print $1}')"
    dst="$(echo "$line" | awk '{print $2}')"
    jq -nc --arg s "$src" --arg d "$dst" '{src:{id:$s},dst:{id:$d}}'
done
