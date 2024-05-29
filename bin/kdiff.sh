#!/bin/bash

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
    "o")
        script="${root}/object.sh" ;;
esac

if [ -z "$script" ] ; then
    name="${0##*/}"
    cat - <<EOS > /dev/stderr
${name} TARGET [ARGS...]

h : helm_diff.sh
hb: helm_diff_between_branches.sh
k : kustomize_diff.sh
kb: kustomize_diff_between_branches.sh
o : object.sh
EOS
    exit 1
fi

shift
"$script" "$@"
