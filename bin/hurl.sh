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

Envs:
  HURL_DRY:
    dry run if set
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
        "h" | "hj" | "hjson")
            cmd="${cmd} -w %{header_json}"
            ;;
        *)
            __help
            return 1
            ;;
    esac
    cmd="${cmd} $*"
    if [ -n "$HURL_DRY" ] ; then
        echo "$cmd"
    else
        $cmd
    fi
}

main "$@"
