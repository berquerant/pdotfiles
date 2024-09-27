#!/bin/zsh

alias k='kubectl'
alias kk='kubectl kustomize'

kgetall() {
    kubectl get "$(kubectl api-resources --namespaced=true --verbs=list --output=name | xargs | tr ' ' ',')" $@
}

kgetevent() {
    # kgetevent [MINUTE] [KUBECTL_OPT...]
    local minute="${1:-10}"
    local second="$(($minute*60))"
    shift
    kubectl get event --sort-by='.lastTimestamp' $@  -o json |\
        jq --arg now $(date +%s) \
           --arg second $second \
           '.items[] | select((.lastTimestamp | fromdate) > (($now | tonumber) - ($second | tonumber)))' -c
}
