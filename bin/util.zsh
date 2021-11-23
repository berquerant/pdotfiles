#!/bin/zsh

source $DOTFILES_ROOT/bin/common.sh

countfd() {
    lsof | awk '{print $1}' | sort | uniq -c | sort -n
}

getlocaladdr() {
    netstat -rn -f inet | awk '$1 == "default" {print $4}' | xargs ipconfig getifaddr
}

# find files or directories >= 5GB
diskcheck() {
    sudo du -m -x -d 5 / 2> /dev/null | awk '$1 >= 5000' | awk '{print $1, length($2), $2}' | sort -rn | awk '{print $1, $3}'
}

# translate CR into NL
map_nl() {
    stty icrnl
}
demap_nl() {
    stty -icrnl
}

# find-merge COMMIT BRANCH
find-merge() {
    local commit
    local branch
    commit="$1"
    branch="${2:-HEAD}"
    (git rev-list "$commit".."$branch" --ancestry-path | cat -n; git rev-list "$commit".."$branch" --first-parent | cat -n) | sort -k2 -s | uniq -f1 -d | sort -rn | head -1 | cut -f2
}

# show-merge COMMIT BRANCH
show-merge() {
    local merge
    merge=`find-merge "$1" "$2"`
    if [ -n "$merge" ]
    then
        git show "$merge"
    else
        cecho red "cannot find commit:$1 branch:$2" >&2
    fi
}

# bisect-run START_COMMIT END_COMMIT COMMAND
bisect-run() {
    if [ $# -le 2 ]
    then
        cecho red "not enough arguments" >&2
        return 1
    fi
    local start
    local end
    start=$1
    end=$2
    shift 2
    git bisect start "$start" "$end"
    git bisect run $@
}

alias gfindmr=find-merge
alias gshowmr=show-merge
