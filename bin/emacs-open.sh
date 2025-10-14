#!/bin/bash

#
# emacs-open.sh [FILE[:LINE]]
#
readonly target="$1"
readonly raw="$RAW"
if [[ -z "$raw" ]] ; then
    readonly target_string="${PWD}/${target}"
else
    readonly target_string="$target"
fi
readonly target_file="${EMACS_OPEN_FILE_TARGET:-.emacs-open-file-target}"
echo "$target_string" | tee "$target_file"
if [[ -z "$raw" ]] ; then
    gbrowse -print "$target"
    echo
fi
echo "Call my-open-file-find to open the file"
