#!/bin/bash

set -e

docker run --rm -it docker-man:debian man $@
