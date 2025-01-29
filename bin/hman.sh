#!/bin/bash

readonly __hman_endpoint="https://linux.die.net/man"

main() {
    local -r _page_or_section="$1"
    local -r _page="$2"

    if [ -z "$_page" ] ; then
        open "${__hman_endpoint}/1/${_page_or_section}"
    else
        open "${__hman_endpoint}/${_page_or_section}/${_page}"
    fi
}

main "$@"
