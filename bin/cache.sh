#!/bin/bash

export CACHE_DIR="${TMPD}/.cache-sh"
. "${DOTFILES_ROOT}/ivg/repos/cache-sh/cache.sh"

cache_clear() {
    rm -rf "$CACHE_DIR"
}

cache_custom_encode() {
    zstd | base64
}

cache_custom_decode() {
    base64 -d | zstd -d
}

export CACHE_ENCODE="cache_custom_encode"
export CACHE_DECODE="cache_custom_decode"
