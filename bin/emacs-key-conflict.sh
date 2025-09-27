#!/bin/bash

readonly emacsd="${DOTFILES_ROOT}/.emacs.d"

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

fuzzy_find() {
    "${DOTFILES_ROOT}/bin/fzf.sh" | while read -r line ; do find_key "$line" ; done
}

readonly cmd="$1"
case "$cmd" in
    "l" | "ls" | "list" | "all") list_sequences | fuzzy_find ;;
    *) conflict_sequences | fuzzy_find ;;
esac
