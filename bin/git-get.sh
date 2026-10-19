#!/bin/bash

select_clone() {
  local uri="$1"
  if [ -z "$uri" ]; then
    git-iter get | "${DOTFILES_ROOT}/bin/fzf.sh"
  else
    git-iter get "$uri"
  fi
}

usage() {
  local name="${0##*/}"
  cat - <<EOS >&2
${name} -- clone and change directory

Usage
  ${name}
    Select local repo by git-iter and goto the repo directory

  ${name} URI
    Clone URI and goto the repo directory
EOS
}

if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
  usage
  exit 1
fi

select_clone "$@"
