#!/bin/zsh

# find files or directories >= 5GB
# $1: path (default: /)
# $2: depth (default: 5)
diskcheck() {
    sudo du -m -x -d "${2:-5}" "${1:-/}" 2> /dev/null | awk '$1 >= 5000 {print $1, length($2), $2}' | sort -n | awk '{print $1, $3}'
}

if type bat >/dev/null 2>&1 ; then
    export MANPAGER="sh -c 'col -bx | bat -l man -p'" # colorizing pager for man
    alias bh='bat --plain --language=help' # colorizing help, e.g. rg --help | bh
    batdiff() {
        git diff --name-only --relative --diff-filter=d | xargs bat --diff
    }
else
    alias bat='cat'
fi

if ! type rg >/dev/null 2>&1 ; then
    alias rg='grep'
fi

mkcd() {
    if [[ -z "$1" ]] ; then
        echo "mkcd DIR"
        echo "mkdir -p DIR && cd DIR"
        return
    fi

    mkdir -p "$1" && cd "$1"
}

pcre() {
    if [[ -z "$1" ]] ; then
        echo "pcre REGEX"
        echo "grep STDIN by REGEX"
        return
    fi

    perl -e "while(<>){if(/${1}/){print}}"
}

clean_tmpd() {
    rm -rf "$TMPD" && mkdir -p "$TMPD"
}

__concat_lower() {
    echo "$@" | tr " " "\n" | awk '{acc=acc""tolower($0)}END{print acc}'
}

__join_case() {
    sep="$1"
    shift
    echo "$@" | tr " " "\n" | awk '{print tolower($0)}' | xargs | tr " " "$sep"
}

# e.g. rg -i "$(csgen get input json)"
csgen() {
    echo "($(__concat_lower $@)|$(__join_case _ $@)|$(__join_case \- $@)|($(__join_case " " $@)))"
}

if type gbrowse >/dev/null 2>&1 ; then
    alias gb='gbrowse'
fi

if type cheat >/dev/null 2>&1 ; then
    alias c='cheat'
fi

dman() {
    "${DOTFILES_ROOT}/bin/docker-man.sh" $@
}

alias sqlite-csv='/usr/local/bin/sqlite-csv.sh'
