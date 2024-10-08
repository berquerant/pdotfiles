#!/bin/bash

target="$1"
root="${DOTFILES_ROOT}/ivg/repos/k8s-diff-sh"
script=""
case "$target" in
    "h")
        script="${root}/helm.sh" ;;
    "hb")
        script="${root}/helm_branch.sh" ;;
    "k")
        script="${root}/kustomize.sh" ;;
    "kb")
        script="${root}/kustomize_branch.sh" ;;
    "o")
        script="${root}/object.sh" ;;
    "d")
        script="${root}/diff.sh" ;;
    "b")
        script="${root}/branch.sh" ;;
    "i")
        script="${root}/id.sh" ;;
esac

if [ -z "$script" ] ; then
    name="${0##*/}"
    cat - <<EOS > /dev/stderr
${name} TARGET [ARGS...]

h : helm.sh
hb: helm_branch.sh
k : kustomize.sh
kb: kustomize_branch.sh
o : object.sh
d : diff.sh
b : branch.sh
i : id.sh
EOS
    exit 1
fi

shift
"$script" "$@"
