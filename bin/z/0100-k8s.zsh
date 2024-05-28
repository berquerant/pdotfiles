#!/bin/zsh

alias k='kubectl'
alias kk='kubectl kustomize'

kdiff() {
    target="$1"
    root="${DOTFILES_ROOT}/ivg/repos/k8s-diff-sh"
    script=""
    case "$target" in
        "h")
            script="${root}/helm_diff.sh" ;;
        "hb")
            script="${root}/helm_diff_between_branches.sh" ;;
        "k")
            script="${root}/kustomize_diff.sh" ;;
        "kb")
            script="${root}/kustomize_diff_between_branches.sh" ;;
    esac

    if [ -z "$script" ] ; then
        cat - <<EOS > /dev/stderr
kdiff TARGET [ARGS...]

h : helm_diff.sh
hb: helm_diff_between_branches.sh
k : kustomize_diff.sh
kb: kustomize_diff_between_branches.sh
EOS
        return 1
    fi

    shift
    "$script" "$@"
}

kgetall() {
    kubectl get "$(kubectl api-resources --namespaced=true --verbs=list --output=name | xargs | tr ' ' ',')" $@
}
