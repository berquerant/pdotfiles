#!/bin/bash

clean_docker() {
    docker system prune --force --filter 'until=168h'
    docker volume prune --force
}

clean_brew() {
    brew cleanup --prune 7
}

clean_go() {
    goenv versions | grep -vE "${GO_VERSION}|system" | while read version ; do
        goenv uninstall -f "$version"
    done
}

clean_python() {
    pyenv versions | grep -vE "${PY_VERSION}|system" | while read version ; do
        pyenv uninstall -f "$version"
    done
}

clean_ruby() {
    rbenv versions | grep -vE "${RB_VERSION}|system" | while read version ; do
        rbenv uninstall -f "$version"
    done
}

clean_node() {
    . "${NVM_DIR}/nvm.sh"
    nvm ls --no-colors | grep -E '^ ' | awk '{print $1}' | while read version ; do
        nvm uninstall "$version"
    done
}

set -e
clean_go
clean_python
clean_ruby
clean_node
clean_brew
clean_docker
