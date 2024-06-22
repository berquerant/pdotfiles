#!/bin/bash

# Simple cache database without mutex lock

# cache line format:
# TIMESTAMP\tKEY\tVALUE\tTTL_SECOND

__cache_kv_sep="\t"

__cache_awk() {
    awk -F "$__cache_kv_sep" "$@"
}

__cache_echo() {
    if [ "$(uname)" = "Linux" ] ; then
        # enable interpretation of backslash escapes
        echo -e "$@"
    else
        echo "$@"
    fi
}

# Set new record
#
# $1: cache db file
# $2: key
# $3: value
# $4: ttl second
cache_set() {
    __cache_set_file="$1"
    __cache_set_key="$2"
    __cache_set_value="$3"
    __cache_set_ttl="$4"

    if [ -z "$__cache_set_key" ] || [ -z "$__cache_set_value" ] || [ -z "$__cache_set_ttl" ] ; then
        return 1
    fi

    __cache_set_timestamp="$(__cache_timestamp_now)"

    __cache_echo "${__cache_set_timestamp}${__cache_kv_sep}${__cache_set_key}${__cache_kv_sep}${__cache_set_value}${__cache_kv_sep}${__cache_set_ttl}" >> "$__cache_set_file"
}

__cache_timestamp_now() {
    date +%s
}

__cache_get_raw() {
    __cache_get_raw_file="$1"
    __cache_get_raw_key="$2"
    __cache_get_raw_now="$3"

    __cache_awk -v key="$__cache_get_raw_key" \
                '$2 == key' \
                "$__cache_get_file" |\
        # select latest record
        tail -n 1 |\
        # ignore expired record
        __cache_awk -v now="$__cache_get_raw_now" \
                    'now <= $1 + $4'
}

__cache_get() {
    __cache_get_file="$1"
    __cache_get_key="$2"

    if [ ! -f "$__cache_get_file" ] ; then
       return 1
    fi

    __cache_get_now="$(__cache_timestamp_now)"
    __cache_get_value="$(__cache_get_raw "$__cache_get_file" "$__cache_get_key" "$__cache_get_now")"
    if [ -z "$__cache_get_value" ] ; then
        return 1
    fi
    echo "$__cache_get_value"
}

# Get value by key
#
# $1: cache db file
# $2: key
#
# Exit status is 1 if not found
cache_get() {
    __cache_got="$(__cache_get "$1" "$2")"
    if [ -z "$__cache_got" ] ; then
        return 1
    fi
    echo "$__cache_got" | __cache_awk '{print $3}'
}

# Shrink db file
#
# $1: cache file
cache_vaccum() {
    __cache_vaccum_file="$1"

    __cache_vaccum_keys="$(mktemp)"
    __cache_awk '{print $2}' "$__cache_vaccum_file" | sort -u > "$__cache_vaccum_keys"

    __cache_vaccum_tmp_file="$(mktemp)"
    while read key ; do
        __cache_vaccum_got="$(__cache_get "$__cache_vaccum_file" "$key")"
        if [ -n "$__cache_vaccum_got" ] ; then
            echo "$__cache_vaccum_got" >> "$__cache_vaccum_tmp_file"
        fi
    done < "$__cache_vaccum_keys"

    cat "$__cache_vaccum_tmp_file" > "$__cache_vaccum_file"
}
