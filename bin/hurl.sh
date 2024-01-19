#!/bin/bash

__help() {
    cat - <<EOS
Usage: hurl OP [CURL_OPTS]
OP:
  s, status:
    http code only

  r, response:
    response headers

  v, verbose:
    headers

  j, json:
    info as a json

  h, hjson:
    response headers as a json
EOS
}

main() {
    op="$1"
    if [ -z "$1" ] ; then
        __help
        return 1
    fi

    shift
    cmd='curl -s -o /dev/null'
    case "$op" in
        "s" | "status")
            cmd="${cmd} -w %{http_code}"
            ;;
        "r" | "response")
            cmd="${cmd} -D -"
            ;;
        "v" | "verbose")
            cmd="${cmd} -v"
            ;;
        "j" | "json")
            cmd="${cmd} -w %{json}"
            ;;
        "hj" | "hjson")
            cmd="${cmd} -w %{header_json}"
            ;;
        *)
            __help
            return 1
            ;;
    esac
    cmd="${cmd} $*"
    $cmd
}

main "$@"
