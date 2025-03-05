#!/bin/bash

set -e -o pipefail

svn log "$@" |\
    # find the revision line and first line of the commit message
    rg "^r[0-9]+" -A 2 |\
    # ignore empty lines
    rg -v '^$' |\
    # ignore delimiter lines
    rg -v '^--$' |\
    # combine the revision line and the message
    awk '/lines?$/ {
printf "%s | ", $0
getline
print
next
}'
