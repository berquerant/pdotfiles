#!/bin/bash

sort_yaml() {
    yq --prettyPrint 'sort_keys(..)'
}

helm_sorted() {
    helm template --generate-name "$@" | sort_yaml
}

empty_values() {
    mktemp
}

helm_diff() {
    if [ -z "$1" ] ; then
        echo "helm template and diff"
        echo "helm_diff CHART LEFT_VALUES RIGHT_VALUES [QUERY_LEFT] [QUERY_RIGHT]"
        echo "e.g."
        echo "helm_diff datadog/datadog left_values.yml right_values.yml"
        return
    fi

    target="$1"
    left="$2"
    right="$3"
    query_left="${4}"
    query_right="${5:-$4}"

    if [ -z "$left" ] ; then
        left="$(empty_values)"
    fi
    if [ -z "$right" ] ; then
        right="$(empty_values)"
    fi
    left_result="$(mktemp)"
    right_result="$(mktemp)"
    helm_sorted "$target" --values "$left" | yq "$query_left" > "$left_result"
    helm_sorted "$target" --values "$right" |  yq "$query_right"> "$right_result"
    left_name="${left} ${query_left}"
    right_name="${right} ${query_right}"
    diff -u "$left_result" "$right_result" | sed -e "s|${left_result}|${left_name}|" -e "s|${right_result}|${right_name}|"
}

helm_build() {
    target="$1"
    helm dependencies build "$target"
    helm_sorted "$target"
}

helm_diff_between_branches() {
    if [ -z "$1" ] ; then
        echo "helm build and diff between branches"
        echo "helm_diff_between_branches DIR LEFT_BRANCH RIGHT_BRANCH [QUERY_LEFT] [QUERY_RIGHT]"
        echo "e.g."
        echo "helm_diff_between_branches path/to/chart/dir master changed"
        return
    fi

    target="$1"
    left="$2"
    right="$3"
    query_left="${4}"
    query_right="${5:-$4}"

    original_branch="$(git branch --show-current)"

    left_result="$(mktemp)"
    right_result="$(mktemp)"

    git switch "$left"
    left_sha="$(git rev-parse --short HEAD)"
    helm_build "$target" | yq "$query_left" > "$left_result"

    git switch "$right"
    right_sha="$(git rev-parse --short HEAD)"
    helm_build "$target" | yq "$query_right" > "$right_result"

    left_name="[${left}] ${target} ${query_left}"
    right_name="[${right}] ${target} ${query_right}"
    diff -u "$left_result" "$right_result" | sed -e "s|${left_result}|${left_name}|" -e "s|${right_result}|${right_name}|"

    git switch "$original_branch"
}

kustomize_sorted() {
    kubectl kustomize "$@" | sort_yaml
}

kustomize_diff() {
    if [ -z "$1" ] ; then
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

kustomize_diff_between_branches() {
    if [ -z "$1" ] ; then
        echo "kustomize build and diff between branches"
        echo "kustomize_diff_between_branches DIR LEFT_BRANCH RIGHT_BRANCH [QUERY_LEFT] [QUERY_RIGHT]"
        echo "e.g."
        echo "kustomize_diff_between_branches overlays/env master new 'select(.metadata.name==\"\xxx)'"
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

t="$1"
shift
case "$t" in
    "hd" | "hdiff" | "helm_diff")
        helm_diff $@
        ;;
    "hb" | "hbranch" | "helm_diff_between_branches")
        helm_diff_between_branches $@
        ;;
    "kd" | "kdiff" | "kustomize_diff")
        kustomize_diff $@
        ;;
    "kb" | "kbranch" | "kustomize_branches")
        kustomize_diff_between_branches $@
        ;;
esac
