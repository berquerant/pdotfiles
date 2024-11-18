#!/bin/bash

find_by_shebang() {
    git grep -l "^${1}"
}

find_by_extension() {
    git ls-files | grep "\.${1}$"
}

find_by_interpreter() {
    local interpreter="$1"
    local extension="$2"
    {
        find_by_shebang "#\!/bin/${interpreter}"
        find_by_extension "$extension"
    } | sort -u
}
