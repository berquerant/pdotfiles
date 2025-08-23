#!/bin/bash

while read -r line ; do
    export "$line"
done < "${DOTFILES_ROOT}/.envdir"
