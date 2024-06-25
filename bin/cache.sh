#!/bin/bash

export CACHE_DIR="${TMPD}/.cache-sh/kv/"
. "${DOTFILES_ROOT}/ivg/repos/cache-sh/cache.sh"

cache_clear() {
    rm -rf "$CACHE_DIR"
}
