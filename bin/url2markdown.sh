#!/bin/bash

get() {
    curl -s -L "$@" | markitdown
}

extract_links_from_markdown() {
    grep -oE '\[.*\]\(.*\)'
}

readonly url="$1"
if [ -z "$url" ] ; then
    echo >&2 "URL required"
    exit 1
fi
shift
readonly cmd="$1"
case "$cmd" in
    "links" | "link" | "l")
        get "$url" | extract_links_from_markdown
        ;;
    *)
        get "$url"
        ;;
esac
