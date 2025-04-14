#!/bin/bash

readonly url="$1"
if [ -z "$url" ] ; then
   echo >&2 'no url!'
   exit 1
fi
readonly interval_seconds="${2:-3}"
readonly try_count_limit="${3:-20}"

check() {
    curl -s -o /dev/null "$url"
}

for c in $(seq "$try_count_limit") ; do
    if (( c > 1 )) ; then
        sleep "$interval_seconds"
    fi
    if check ; then
        exit
    fi
done
echo >&2 "healthcheck ${url} failed!"
exit 1
