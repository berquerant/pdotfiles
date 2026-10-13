#!/bin/bash

d=$(
  cd "$(dirname "$0")" || exit 1
  pwd
)
. "${d}/common.sh"

find_by_interpreter bash sh | xargs -n 4 shellcheck -f gcc
