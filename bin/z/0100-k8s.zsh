#!/bin/zsh

alias k='kubectl'
alias kk='kubectl kustomize'

kgetall() {
    kubectl get "$(kubectl api-resources --namespaced=true --verbs=list --output=name | xargs | tr ' ' ',')" $@
}

kgetev() {
    kubectl get event --sort-by='{.lastTimestamp}' $@
}
