#!/bin/zsh

alias k='kubectl'
alias kk='kubectl kustomize'

kustomize_sorted() {
    kubectl kustomize "$@" | yq --prettyPrint 'sort_keys(..)'
}

alias kks='kustomize_sorted'

kustomize_diff() {
    if [[ -z "$1" ]] ; then
        echo "kustomize build and diff"
        echo "kustomize_diff LEFT_DIR RIGHT_DIR [QUERY_LEFT] [QUERY_RIGHT]"
        echo "e.g."
        echo "kustomize_diff overlays/env1 overlays/env2 'select(.metadata.name==\"xxx\")'"
        return
    fi

    left="$1"
    right="$2"
    query_left="${3}"
    query_right="${4:-$3}"

    left_kustomized="$(mktemp)"
    right_kustomized="$(mktemp)"
    kustomize_sorted "$left" | yq "$query_left" > "$left_kustomized"
    kustomize_sorted "$right" | yq "$query_right" > "$right_kustomized"
    left_name="${left} ${query_left}"
    right_name="${right} ${query_right}"
    diff -u "$left_kustomized" "$right_kustomized" | sed -e "s|${left_kustomized}|${left_name}|" -e "s|${right_kustomized}|${right_name}|"
}

alias kd='kustomize_diff'

kustomize_diff_between_branch() {
    if [[ -z "$1" ]] ; then
        echo "kustomize build and diff between branches"
        echo "kustomize_diff_between_branch DIR LEFT_BRANCH RIGHT_BRANCH [QUERY_LEFT] [QUERY_RIGHT]"
        echo "e.g."
        echo "kustomize_diff_between_branch overlays/env master new 'select(.metadata.name==\"\xxx)'"
        return
    fi

    target="$1"
    left="$2"
    right="$3"
    query_left="${4}"
    query_right="${5:-$4}"

    original_branch="$(git branch --show-current)"

    left_kustomized="$(mktemp)"
    right_kustomized="$(mktemp)"

    git switch "$left"
    left_sha="$(git rev-parse --short HEAD)"
    kustomize_sorted "$target" | yq "$query_left" > "$left_kustomized"

    git switch "$right"
    right_sha="$(git rev-parse --short HEAD)"
    kustomize_sorted "$target" | yq "$query_right" > "$right_kustomized"

    left_name="[${left}] ${target} ${query_left}"
    right_name="[${right}] ${target} ${query_right}"
    diff -u "$left_kustomized" "$right_kustomized" | sed -e "s|${left_kustomized}|${left_name}|" -e "s|${right_kustomized}|${right_name}|"

    git switch "$original_branch"
}

alias kdb='kustomize_diff_between_branch'
