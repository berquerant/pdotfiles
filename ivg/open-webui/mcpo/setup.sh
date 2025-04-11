#!/bin/sh

set -ex
apk add python3 py3-pip py3-virtualenv
python3 -m venv tools
. tools/bin/activate
pip install -U pip
pip install mcpo
