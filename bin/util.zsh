#!/bin/zsh

# find files or directories >= 5GB
diskcheck() {
    sudo du -m -x -d 5 / 2> /dev/null | awk '$1 >= 5000' | awk '{print $1, length($2), $2}' | sort -rn | awk '{print $1, $3}'
}

if type bat >/dev/null 2>&1 ; then
    export MANPAGER="sh -c 'col -bx | bat -l man -p'" # colorizing pager for man
    alias bathelp='bat --plain --language=help'  # colorizing help, e.g. rg --help | bathelp
    belp() {
        if [[ -z "$1" ]] ; then
            echo "belp COMMAND [HELP]"
            echo "  COMMAND:"
            echo "    a command to be displayed manual"
            echo "    belp COMMAND runs COMMAND --help | bathelp"
            echo "  HELP:"
            echo "    specify COMMAND's help option"
            echo "    belp COMMAND HELP runs COMMAND HELP | bathelp"
            return
        fi

        local cmd="$1"
        local help_opt="--help"
        if [[ -n "$2" ]] ; then
            help_opt="$2"
        fi
        $cmd $help_opt | bat --plain --language=help
    }
    batdiff() {
        git diff --name-only --relative --diff-filter=d | xargs bat --diff
    }
fi
