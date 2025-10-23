#!/bin/zsh

alias k='kubectl'
alias kk='kubectl kustomize'
alias ksdiff='${DOTFILES_ROOT}/bin/k8s-sync-diff.sh'
alias kapi='${DOTFILES_ROOT}/bin/k8s-api.sh'
alias kr='kubectl get --raw'
alias kv='kubectl -v=8'

kgetall() {
    kubectl get "$(kubectl api-resources --namespaced=true --verbs=list --output=name | xargs | tr ' ' ',')" $@
}

kgetevent() {
    # kgetevent [MINUTE] [KUBECTL_OPT...]
    local minute="$1"
    shift
    local second="$(($minute*60))"
    local now="$(date +%s)"
    local threshold="$(($now - $second))"
    kubectl get event $@  -o json |\
        jq --arg threshold $threshold \
           -c \
           '[.items[] | (.lastTimestamp // (.eventTime | sub("\\.[^Z]+"; ""))) as $t | . + {"t": ($t | fromdate), "tt": $t} | select(.t > ($threshold | tonumber))] | sort_by(.t) | .[]'
}
