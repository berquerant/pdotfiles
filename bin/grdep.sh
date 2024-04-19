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

gen_env_config_from_zshrc() {
    # extract environment variables
    grep -E '^export' "${DOTFILES_ROOT}/.zshrc" |\
        cut -d "=" -f 1 |\
        cut -d " " -f 2 |\
        sort -u |\
        jq --slurp --raw-input '{node:[split("\n")[:-1][]|{name: ., category: ".*", matcher: [{r: .}, {val: [.]}]}]}' |\
        cv json yaml |\
        yq --prettyPrint 'sort_keys(..)'
}

usage() {
    name="${0##*/}"
    cat - <<EOS
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
                gen_env_config_from_zshrc
                ;;
            *)
                gen_env_config_from_zshrc > "${DOTFILES_ROOT}/grdep/0010-generated-env-zshrc.yml"
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
