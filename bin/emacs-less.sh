#!/bin/bash

tmp="$(mktemp)"
cat - > "$tmp"
"${DOTFILES_ROOT}/bin/emacs-light.sh" "$tmp" < /dev/tty
