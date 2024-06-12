#!/bin/bash

find_config() {
    find "${DOTFILES_ROOT}/grdep" -type f -name "*.yml" | sort | xargs
}

as_json2dot() {
    jq '{src: {id: .line.path}, dst: {id: .node.normalized.result}}' -c
}

__grdep() {
    git ls-files | grdep run $(find_config) "$@"
}

gen_env_config() {
    # extract environment variables
    echo ".envrc .zshrc $(find bin/z -type f -name "*.zsh" | sort | xargs)" |\
        tr ' ' '\n' |\
        awk -v r="$DOTFILES_ROOT" '{print r"/"$0}' |\
        xargs -n 1 grep -E '^export' |\
        cut -d "=" -f 1 |\
        cut -d " " -f 2 |\
        sort -u |\
        jq --slurp --raw-input '{node:[split("\n")[:-1][]|{name: ., category: ".*", matcher: [{r: .}, {val: [.]}]}]}' |\
        cv json yaml |\
        yq --prettyPrint 'sort_keys(..)'
}

usage() {
    name="${0##*/}"
    cat - <<EOS >&2
${name} -- grdep the repository

Usage
  ${name} [GRDEP_OPTS]
    grdep the repo with configs under grdep/.

  ${name} gen
    Generate grdep config.

  ${name} json2dot
    Generate json2dot input.

  ${name} h|help
    Show this help.

Example
  ${name} json2dot | json2dot --fontsize_max 96 | edot svg -Ksfdp -Goverlap=scalexy -Gsize=90
EOS
}

cd $DOTFILES_ROOT
case "$1" in
    "json2dot")
        shift
        __grdep "$@" | as_json2dot
        ;;
    "gen")
        shift
        case "$1" in
            "dry")
                gen_env_config
                ;;
            *)
                gen_env_config > "${DOTFILES_ROOT}/grdep/0010-generated-env.yml"
                ;;
        esac
        ;;
    "h" | "help")
        usage
        exit 1
        ;;
    *)
        __grdep "$@"
        ;;
esac
