#!/bin/bash

docker_prune() {
    docker system prune --filter 'until=168h'
    docker volume prune --force
}

set -ex
docker_prune
