#!/bin/bash

d=$(cd $(dirname $0)/..; pwd)
target="$1"

req() {
    echo "${d}/requirements/${1}.txt"
}

install_python() {
    pip install --upgrade pip setuptools wheel
    pip install -r "$1"
}

install_go() {
    set +e
    brew uninstall go
    set -e
    cat "$1" | while read pkg ; do go install "$pkg" ; done
}

install_gem() {
    cat "$1" | while read line ; do gem install $line ; done
}

install_node() {
    set +e
    npm install -g npm@latest
    set -e
    cat "$1" | xargs npm install -g
}

install_cargo() {
    cat "$1" | xargs cargo install
}

install_rustup() {
    rustup self update
    rustup install stable
    rustup override set stable
    rustup update nightly
    rustup toolchain add nightly
    cat "$1" | xargs rustup component add
}

set -ex

req_file="$(req $target)"

case "${target}" in
    "cargo")
        install_cargo "$req_file"
        ;;
    "rustup")
        install_rustup "$req_file"
        ;;
    "rust")
        install_rustup "$(req rustup)"
        install_cargo "$(req cargo)"
        ;;
    "python")
        install_python "$req_file"
        ;;
    "go")
        install_go "$req_file"
        ;;
    "gem")
        install_gem "$req_file"
        ;;
    "ruby")
        install_gem "$(req gem)"
        ;;
    "node")
        install_node "$req_file"
        ;;
    "all")
        install_python "$(req python)"
        install_go "$(req go)"
        install_node "$(req node)"
        install_gem "$(req gem)"
        install_cargo "$(req cargo)"
        install_rustup "$(req rustup)"
        ;;
    *)
        echo "Unknown target ${target}" >&2
        exit 1
        ;;
esac
