#!/bin/bash

set -euo pipefail

d="$(
  cd "$(dirname "$0")"/.. || exit 1
  pwd
)"
. "${d}/bin/common.sh"

loader="$(mktemp)"
trap 'rm -f "$loader"' EXIT

"${d}/bin/emacs-extract-modes.sh" -f elisp > "$loader"
keybind-dump --load "$loader" -s '^C-.+' -s '^M-.+' -f jsonl | keybind-dump - "$@"
