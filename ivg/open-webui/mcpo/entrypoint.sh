#!/bin/sh

. tools/bin/activate
mcpo --port 8000 --host 0.0.0.0 --config ./config.json
