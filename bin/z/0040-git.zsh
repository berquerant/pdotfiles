#!/bin/zsh

export GIT_EDITOR='vim'
export GHQ_ROOT=$HOME/go/src
export GIT_USER=`git config user.name`
alias g='git'

alias repos='ghq list -p | peco'
alias repo='cd $(repos)'
alias repopath='ghq list | peco | cut -d "/" -f 2,3'

grepo() {
    if [[ -z "$1" ]] ; then
        repo && bat $(git ls --full-name | peco)
    else
        repo && rg "$@" $(git ls --full-name | peco)
    fi
}

ggrepo() {
    if [[ -z "$1" ]] ; then
        echo "ggrepo GIT_GREP_OPTIONS"
        return
    fi
    repo && git grep "$@"
}

gfswitch() {
    if [[ -z "$1" ]] ; then
        echo "git switch, forcely"
        echo "gfswitch BRANCH"
        return
    fi
    git branch -D "$1" && git fetch origin "$1" && git switch "$1"
}

gfbranch() {
    if [[ -z "$1" ]] ; then
        echo "git checkout -b, forcely"
        echo "gfbranch BRANCH"
        return
    fi
    git branch -D $1 && git checkout -b $1
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
