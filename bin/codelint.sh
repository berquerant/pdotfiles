#!/bin/bash

log() {
  echo >&2 "--------------------------------------------------"
  echo >&2 "$*"
  echo >&2 "--------------------------------------------------"
}

result=0
run() {
  log "RUN: $*"
  if ! "$@" ; then
    result=1
  fi
  log "EXIT WITH ${result} ($*)"
}

exclude_dirs=".git .hg .svn"
if [[ -n "$*" ]] ; then
  exclude_dirs="${exclude_dirs} $*"
fi

lizard_excludes=()
for d in $exclude_dirs; do
  lizard_excludes+=(-x "./${d}/*")
done

run scc --by-file -s complexity --exclude-dir "${exclude_dirs// /,}"
run lizard -w -C 15 -L 100 -s cyclomatic_complexity "${lizard_excludes[@]}"

exit "$result"
