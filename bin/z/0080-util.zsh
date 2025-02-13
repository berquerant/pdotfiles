#!/bin/zsh

# find files or directories >= 5GB
# $1: path (default: /)
# $2: depth (default: 5)
diskcheck() {
    sudo du -m -x -d "${2:-5}" "${1:-/}" 2> /dev/null | awk '$1 >= 5000 {print $1, length($2), $2}' | sort -n | awk '{print $1, $3}'
}

if type gsed >/dev/null 2>&1 ; then
    alias sed='gsed'
fi

if ! type rg >/dev/null 2>&1 ; then
    alias rg='grep'
fi

mkcd() {
    if [[ -z "$1" ]] ; then
        echo "mkcd DIR"
        echo "mkdir -p DIR && cd DIR"
        return 1
    fi

    mkdir -p "$1" && cd "$1"
}

pcre() {
    if [[ -z "$1" ]] ; then
        echo "pcre REGEX"
        echo "grep STDIN by REGEX"
        return 1
    fi

    perl -e "while(<>){if(/${1}/){print}}"
}

clean_tmpd() {
    rm -rf "$TMPD" && mkdir -p "$TMPD"
}

# e.g. rg -i "$(csg get input json)"
csg(){
    seed=' |->|>|.|:|/|_|\-'
    if [ -n "${CSG}" ] ; then
        seed="${CSG}"
    fi
    sep="(${seed})*"
    echo "$@" | tr " " "\n" | awk '{print tolower($0)}' | xargs | sed "s^ ^${sep}^g"
}

if type gbrowse >/dev/null 2>&1 ; then
    alias gb='gbrowse'
fi

hurl() {
    "${DOTFILES_ROOT}/bin/hurl.sh" $@
}

jmerge() {
    "${DOTFILES_ROOT}/bin/merge.sh" j $@
}

ymerge() {
    "${DOTFILES_ROOT}/bin/merge.sh" y $@
}

jsort() {
    jq --sort-keys
}

ysort() {
    yq --prettyPrint 'sort_keys(..)'
}

edot() {
    "${DOTFILES_ROOT}/bin/dot.sh" $@
}

unzip_via_ditto() {
    ditto -V -x -k --sequesterRsrc "$@"
}

source "${DOTFILES_ROOT}/bin/cache.sh"

reload_tmpd() {
    rm -rf "$TMPD"
    mkdir -p "$TMPD"
}

checksum() {
    "${DOTFILES_ROOT}/bin/checksum.sh" "$@"
}

hman() {
    "${DOTFILES_ROOT}/bin/hman.sh" "$@"
}

dman() {
    dmanraw debian "$@"
}

dmanraw() {
    "${DOTFILES_ROOT}/bin/dman.sh" "$@"
}

mln() {
    "${DOTFILES_ROOT}/bin/mln.sh" "$@"
}
