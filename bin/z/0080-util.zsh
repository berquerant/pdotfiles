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
    echo "$@" | tr " " "\n" | awk '{print tolower($0)}' | xargs | sed "s^ ^${sep}^g"
}

# e.g. rg -i "$(csg get input json)"
csg() {
    echo "($(__concat_lower $@)|$(__join_case "->" $@)|$(__join_case "\\\." $@)|$(__join_case ":" $@)|$(__join_case "::" $@)|$(__join_case "/" $@)|$(__join_case _ $@)|$(__join_case \- $@)|($(__join_case " " $@)))"
}

if type gbrowse >/dev/null 2>&1 ; then
    alias gb='gbrowse'
fi

if type cheat >/dev/null 2>&1 ; then
    alias c='cheat'
fi

dman() {
    image=docker-man:debian
    case "$1" in
        "d")
            shift
            ;;
        "u")
            shift
            image=docker-man:ubuntu
            ;;
    esac
    "${DOTFILES_ROOT}/bin/docker-rmit.sh" "$image" "$@"
}

alias sqlite-csv='/usr/local/bin/sqlite-csv.sh'

dterraform() {
    "${DOTFILES_ROOT}/bin/docker-rmit.sh" hashicorp/terraform:latest "$@"
}

hurl() {
    op="$1"
    if [ -z "$1" ] ; then
        cat - <<EOS
Usage: hurl OP [CURL_OPTS]
OP:
  s, status:
    http code only

  r, response:
    response headers

  v, verbose:
    headers

  j, json:
    info as a json

  h, hjson:
    response headers as a json
EOS
        return 1
    fi

    shift
    case "$op" in
        "s" | "status")
            curl -s -o /dev/null -w "%{http_code}" "$@"
            ;;
        "r" | "response")
            curl -D - -s -o /dev/null "$@"
            ;;
        "v" | "verbose")
            curl -v -s -o /dev/null "$@"
            ;;
        "j" | "json")
            curl -s -o /dev/null -w '%{json}' "$@"
            ;;
        "h" | "hjson")
            curl -s -o /dev/null -w '%{header_json}' "$@"
            ;;
        *)
            return 1
            ;;
    esac
}
