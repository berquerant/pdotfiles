#!/bin/bash

test_log() {
    echo "[test] $*" > /dev/stderr
}

test_run() {
    test_log "Start $1"
    "$1"
    ret=$?
    test_log "End $1"
    return $ret
}

test_run_multi() {
    result="$(mktemp)"

    ret=0
    while [ -n "$1" ] ; do
        test_run $1
        r=$?
        if [ $r -gt 0 ] ; then
            ret=$r
        fi
        echo "$1 ${r}" >> "$result"
        shift
    done

    test_log "----------"
    cat "$result" > /dev/stderr
    return $ret
}


thisd="$(cd $(dirname $0); pwd)"
. "${thisd}/../../bin/cache.sh"

test_cache_util_get_failure() {
    test_log "test_cache_util_get_failure $*"
    ! cache_get "$1" "$2" > /dev/null
}
test_cache_util_get_success() {
    test_log "test_cache_util_get_success $*"
    if ! cache_get "$1" "$2" > /dev/null ; then
        return 1
    fi
    cache_util_get_value="$(cache_get $1 $2)"
    [ "$cache_util_get_value" = "$3" ]
}
test_cache_util_check_lines() {
    test_log "test_cache_util_check_lines $*"
    cache_file_lines="$(wc -l $1 | awk '{print $1}')"
    [ "$cache_file_lines" = $2 ]
}

test_cache_scenario() {
    set -e
    cache_file="$(mktemp)"

    # no keys yet
    test_cache_util_get_failure "$cache_file" "key1"
    # set key1
    cache_set "$cache_file" "key1" "value1" 300
    # get key1
    test_cache_util_get_success "$cache_file" "key1" "value1"
    # set key2
    cache_set "$cache_file" "key2" "value2" 300
    # update key1
    cache_set "$cache_file" "key1" "value1_2" 300
    # get key1
    test_cache_util_get_success "$cache_file" "key1" "value1_2"
    # get key2
    test_cache_util_get_success "$cache_file" "key2" "value2"
    # check file lines
    test_cache_util_check_lines "$cache_file" 3
    # try vaccum
    cache_vaccum "$cache_file"
    # check file lines
    test_cache_util_check_lines "$cache_file" 2
    # get key1
    test_cache_util_get_success "$cache_file" "key1" "value1_2"
    # get key2
    test_cache_util_get_success "$cache_file" "key2" "value2"
    # set key3 with ttl 2 second
    cache_set "$cache_file" "key3" "value3" 2
    # get key3
    test_cache_util_get_success "$cache_file" "key3" "value3"
    # expire key3
    sleep 3
    test_cache_util_get_failure "$cache_file" "key3"
    # update key3
    cache_set "$cache_file" "key3" "value3_2" 300
    # get key3
    test_cache_util_get_success "$cache_file" "key3" "value3_2"
    # update key3 with ttl 2 second
    cache_set "$cache_file" "key3" "value3_3" 2
    # get key3
    test_cache_util_get_success "$cache_file" "key3" "value3_3"
    # expire key3
    sleep 3
    test_cache_util_get_failure "$cache_file" "key3"
    # check file lines
    test_cache_util_check_lines "$cache_file" 5
}

test_run_multi "test_cache_scenario"
