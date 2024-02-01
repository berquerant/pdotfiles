#!/bin/bash

clean_docker() {
    docker system prune --filter 'until=168h'
    docker volume prune --force
}

clean_brew() {
    brew cleanup --prune 7
}

set -ex
clean_brew
clean_docker
