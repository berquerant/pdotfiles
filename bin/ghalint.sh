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

run pinact run
run ghalint run
run actionlint
run zizmor --persona=auditor ./

exit "$result"
