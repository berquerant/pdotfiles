#!/bin/bash

d=$(cd $(dirname $0)/..; pwd)
python "${d}/bin/guesslang.py" "$@"
