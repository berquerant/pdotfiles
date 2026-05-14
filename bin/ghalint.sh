#!/bin/bash

result=0
run() {
  echo >&2 "--------------------------------------------------"
  echo >&2 "RUN: $*"
  echo >&2 "--------------------------------------------------"
  if ! "$@" ; then
    result=1
  fi
}

run pinact run
run ghalint run
run actionlint
run zizmor --persona=auditor ./

exit "$result"
