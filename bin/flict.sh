#!bin/bash

case "$1" in
    c)
        shift
        flict display-compatibility "$@"
        ;;
    cd)
        shift
        flict -of dot display-compatibility "$@"
        ;;
    o)
        shift
        # shellcheck disable=SC2046,SC2001
        flict outbound-candidate $(echo "$*" | sed 's| | AND |g')
        ;;
    *)
        name="${0##*/}"
        cat <<EOS >&2
${name} -- flict utility

Usage
  ${name} c ...
    display-compatibility

  ${name} cd ...
    display-compatibility via dot

  ${name} o ...
     outbound-candidate
EOS
esac
