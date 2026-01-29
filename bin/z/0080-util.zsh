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

alias gb='gbrowse'
alias jsort='jq --sort-keys'
alias ysort='yq --prettyPrint "sort_keys(..)"'
alias edot='${DOTFILES_ROOT}/bin/dot.sh'
alias unzip_via_ditto='ditto -V -x -k --sequesterRsrc'

source "${DOTFILES_ROOT}/bin/cache.sh"

reload_tmpd() {
    rm -rf "$TMPD"
    mkdir -p "$TMPD"
}

alias checksum='${DOTFILES_ROOT}/bin/checksum.sh'
alias hman='${DOTFILES_ROOT}/bin/hman.sh'
alias mln='${DOTFILES_ROOT}/bin/mln.sh'
alias drun='${DOTFILES_ROOT}/bin/docker.sh'
alias rrg='rg --hidden'
alias dc='${DOTFILES_ROOT}/bin/devcontainer.sh'

if which ndql >/dev/null 2>&1 ; then
    alias nq='ndql query'
fi
