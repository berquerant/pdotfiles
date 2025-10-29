#!/bin/bash

if [[ $# -gt 0 ]] ; then
    git ls-files --deduplicate | rg "$@"
else
    git ls-files --deduplicate
fi
