#!/bin/zsh

export GIT_EDITOR='vim'
export GHQ_ROOT=$HOME/go/src
export GIT_USER=`git config user.name`
alias g='git'

alias repos='ghq list -p | peco'
alias repo='cd $(repos)'
alias repopath='ghq list | peco | cut -d "/" -f 2,3'

gfbranch() {
    if [[ -z "$1" ]] ; then
        echo "switch branch if remote branch exists else create branch, forcely"
        echo "gfbranch BRANCH"
        return
    fi

    branch="$1"
    if git branch | grep -q "$branch" ; then
        git branch -D "$branch"
    fi
    git fetch
    git switch "$branch" || git checkout -b "$branch"
}

grepopath() {
    git config --get remote.origin.url |\
        tr ":" "/" |\
        sed -E 's|git@|https///|' |\
        sed -E 's|git///|https///|' |\
        sed -E 's|\.git||' |\
        sed -E 's|https///|https://|' |\
        sed -E 's|https://||'
}

export GIT_WORKTREE_ROOT="$GHQ_ROOT/git-worktree"
gwadd() {
    worktree_prefix="${GIT_WORKTREE_ROOT}/$(grepopath)"
    if [[ -z "$1" ]] ; then
        echo "git worktree add, prefix is ${worktree_prefix}"
        echo "gwadd BRANCH [OPTION]"
        return
    fi
    worktree_path="${worktree_prefix}/${1}"
    echo "${worktree_path}"
    git worktree add "${worktree_path}" "$@"
}

alias gwrm='git worktree remove'
alias gwls='git worktree list'

gsubremove() {
    if [[ -z "$1" ]] ; then
        echo "remove submodule"
        echo "gsubremove MODULE"
        return
    fi
    git submodule deinit -f "$1" && git rm -f "$1" && rm -rf .git/modules/ "$1"
}

gfreset() {
    if [[ -z "$1" ]] ; then
        echo "sync to remote branch"
        echo "gfreset BRANCH"
        return
    fi
    git fetch && git reset --hard "origin/$1"
}

ggdo() {
    if [[ -z "$1" ]] ; then
        echo "run command in multiple repositories"
        echo "ggdo REPO_REGEX CMD"
        echo "use bash -c to execute CMD, but when GGDO_RAW is not empty, execute CMD as is"
        return
    fi

    repo_regex="$1"
    shift

    ghq list -p | rg "$repo_regex" | while read line ; do
        pushd "$line" > /dev/null
        if [[ -n "$GGDO_RAW" ]] ; then
            "$@"
        else
            bash -c "$@"
        fi
        popd > /dev/null
    done
}

__gggrep_run() {
    groot="$(ghq root)/"
    sed_expr="s|${groot}||"
    repo_path="$(pwd|sed $sed_expr)"
    git grep -H "$@" | awk -v r="$repo_path" '{print r"/"$0}'
}

gggrep() {
    if [[ -z "$1" ]] ; then
        echo "grep multiple repositories"
        echo "gggrep REPO_REGEX [OPT...] REGEX"
        return
    fi

    repo_regex="$1"
    shift
    GGDO_RAW=1 ggdo "$repo_regex" __gggrep_run "$@"
}

rpeep() {
    if [[ -z "$1" ]] ; then
        repo && bat $(git ls --full-name | peco)
    else
        repo && rg "$@" $(git ls --full-name | peco)
    fi
}

rgrep() {
    if [[ -z "$1" ]] ; then
        echo "rgrep GIT_GREP_OPTIONS"
        return
    fi
    repo && git grep "$@"
}
