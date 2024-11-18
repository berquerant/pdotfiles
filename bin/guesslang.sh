#!/bin/bash

d="$(cd "$(dirname "$0")"/.. || exit; pwd)"
python "${d}/bin/guesslang.py" "$@"
