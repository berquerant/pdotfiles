#!/bin/bash

readonly d="$(cd "$(dirname "$0")"/.. || exit 1; pwd)"
readonly emacsd="${d}/.emacs.d"

find_keys() {
    git grep -o '\(unbind-key "[^"]+"' "$emacsd"
    git grep -o '\(bind-key "[^"]+"' "$emacsd"
    git grep -o '\("[CM][^"]+" \.' "$emacsd"
}

find_key() {
    local -r target="$1"
    git grep "\(unbind-key \"${target}[^\"]*\"" "$emacsd"
    git grep "\(bind-key \"${target}[^\"]*\"" "$emacsd"
    git grep "\(\"${target}[^\"]*\" \." "$emacsd"
}

extract_sequence() {
    cut -d '"' -f2- | rev | cut -d '"' -f2- | rev
}

list_sequences() {
    find_keys | extract_sequence
}

conflict_sequences() {
    list_sequences | sort | uniq -c | awk '$1 > 1' | sed 's|^ *||' | cut -d " " -f 2-
}

grep_sequence() {
    while read -r line ; do find_key "$line" ; done
}

fuzzy_find() {
    "${d}/bin/fzf.sh" | grep_sequence
}

usage() {
    local -r name="${0##*/}"
    cat - <<EOS
${name} (l|ls|list|all)
  Find key bindings from all sequences
${name} (s|seq) SEQUENCE
  Find key bindings by SEQUENCE
${name} (r|raw)
  Find all key bindings with conflict
${name}
  Find key bindings with conflict
EOS
}

readonly cmd="$1"
case "$cmd" in
    "l" | "ls" | "list" | "all") list_sequences | fuzzy_find ;;
    "s" | "seq")
        if [[ -z "$2" ]] ; then
            usage
            exit 1
        fi
        find_key "$2"
        ;;
    "r" | "raw") conflict_sequences ;;
    "-h" | "--help")
        usage
        exit 1
        ;;
    *) conflict_sequences | fuzzy_find ;;
esac
