#!/bin/zsh

alias k='kubectl'
alias kk='kubectl kustomize'

kustomize_diff() {
    "${DOTFILES_ROOT}/bin/k8s-diff.sh" kd $@
}

kustomize_branch() {
    "${DOTFILES_ROOT}/bin/k8s-diff.sh" kb $@
}

helm_diff() {
    "${DOTFILES_ROOT}/bin/k8s-diff.sh" hd $@
}

helm_branch() {
    "${DOTFILES_ROOT}/bin/k8s-diff.sh" hb $@
}
