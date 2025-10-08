#!/bin/bash

readonly config="${CONFIG:-renovate.json}"

docker run --rm -it \
       -e LOG_LEVEL=debug \
       -e RENOVATE_CONFIG_FILE="$config" \
       -v "$PWD:/app/src" \
       -w "/app/src" \
       renovate/renovate \
       renovate-config-validator --strict \
       "$@"
