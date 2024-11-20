#!/bin/bash

d="$(cd "$(dirname "$0")"/.. || exit; pwd)"
target="$1"

req() {
    echo "${d}/requirements/${1}"
}

__ignore_comment() {
    grep -v -E '^#'
}

__install_from_file() {
    local target_file="$1"
    shift
    local t
    t="$(mktemp)"
    __ignore_comment  < "$target_file" > "$t"
    "$@" "$t"
}

__install_from_lines() {
    local target_file="$1"
    shift
    # shellcheck disable=SC2086
    __ignore_comment < "$target_file" | while read -r x ; do "$@" $x ; done
}

install_python() {
    pip install --upgrade pip setuptools wheel
    __install_from_file "$1" pip install -r
}

install_go() {
    set +e
    brew uninstall go
    set -e
    __install_from_lines "$1" go install
}

install_gem() {
    __install_from_lines "$1" gem install
}

install_node() {
    set +e
    npm install -g npm@latest
    set -e
    __install_from_lines "$1" npm install -g
}

install_cargo() {
    __install_from_lines "$1" cargo install
}

install_rustup() {
    rustup self update
    rustup install stable
    rustup override set stable
    rustup update nightly
    rustup toolchain add nightly
    __install_from_lines "$1" rustup component add
}

set -ex

req_file="$(req "$target")"

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
        cat <<EOS >&2
$0 CATEGORY

install requirements, available categories:

- cargo
- rustup
- rust
- python
- go
- gem
- ruby
- node
- all

requirements files are in ${d}/requirements,
lines with # at the beginning are comments
EOS
        exit 1
        ;;
esac
