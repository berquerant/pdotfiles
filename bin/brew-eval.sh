#!/bin/bash

__arch="$(uname -m)"
if [ "$__arch" = "arm64" ]; then
    echo "${__arch} uses /opt/homebrew/bin/brew"
    export PATH="$PATH:/opt/homebrew/bin"
    eval "$(/opt/homebrew/bin/brew shellenv)"
elif [ "$__arch" = "x86_64" ]; then
    echo "${__arch} uses /usr/local/bin/brew"
    export PATH="$PATH:/usr/local/bin"
    eval "$(/usr/local/bin/brew shellenv)"
fi
