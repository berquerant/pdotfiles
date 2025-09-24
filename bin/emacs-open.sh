#!/bin/bash

#
# emacs-open.sh [FILE[:LINE]]
#
readonly target="$1"
readonly target_string="${PWD}/${target}"
readonly target_file="${EMACS_OPEN_FILE_TARGET:-.emacs-open-file-target}"
echo "$target_string" | tee "$target_file"
gbrowse -print "$target"
echo
echo "Call my-open-file-find to open the file"
