#!/bin/bash
# commmon functions

# translate color name into a number for tput.
#
# $1 : color name
cname2num() {
    case "$1" in
        "black")
            echo 0 ;;
        "red")
            echo 1 ;;
        "green")
            echo 2 ;;
        "yellow")
            echo 3 ;;
        "blue")
            echo 4 ;;
        "magenta")
            echo 5 ;;
        "cyan")
            echo 6 ;;
        *)
            echo 7 ;;
    esac
}

# echo with colored characters.
#
# $1 : foreground color or forground color/background color
# $2 : message
cecho() {
    if echo "$1" | grep -q "/"
    then
        tput setaf "$(cname2num "$(echo "$1" | cut -d "/" -f 1)")"
        tput setab "$(cname2num "$(echo "$1" | cut -d "/" -f 2)")"
    else
        tput setaf "$(cname2num "$1")"
    fi
    echo "$2"
    tput sgr0
}

# ask whether execute or not.
# exit if denied.
#
# $1 : required. query statement.
# $2 : statement when denied.
exec_query() {
    local yn
    read -r -n1 -p "$1 (y/N): " yn
    echo
    if [[ $yn != [yY] ]]
    then
        if [ -n "$2" ] ; then echo "$2" ; fi
        exit
    fi
}

# run command or echo command.
#
# $1 : required. command.
# $2 : echo $1 if true else run $1.
run_or_dry() {
    if $2
    then
        echo "$1"
    else
        echo "$1" | bash
    fi
}

# get current time with ms accuracy.
stopwatch() {
    gdate +"%F %T.%N"
}

# mkdir if not exists, and cd.
#
# $1 : required. directory.
ensure_cd() {
    mkdir -p "$1"
    cd "$1" || return 1
}

# remove extension
#
# $1 : required
rmext() {
    echo "${1%.*}"
}

# get extension
#
# $1 : required
getext() {
    echo "${1##*.}"
}
