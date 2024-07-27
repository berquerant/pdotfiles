#!/bin/bash

__arch="$(uname -m)"
if [ "$__arch" = "arm64" ]; then
    echo "${__arch} uses /opt/homebrew/bin/brew"
    eval "$(/opt/homebrew/bin/brew shellenv)"
elif [ "$__arch" = "x86_64" ]; then
    echo "${__arch} uses /usr/local/bin/brew"
    eval "$(/usr/local/bin/brew shellenv)"
fi
