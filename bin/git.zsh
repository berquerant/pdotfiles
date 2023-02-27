#!/bin/zsh

export GIT_EDITOR='vim'
export GHQ_ROOT=$HOME/go/src
export GIT_USER=`git config user.name`
alias g='git'

alias repos='ghq list -p | peco'
alias repo='cd $(repos)'
alias repopath='ghq list | peco | cut -d "/" -f 2,3'
alias rrepo='hub browse $(repopath)'

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

gfpull() {
    if [[ -z "$1" ]] ; then
        echo "git pull, forcely"
        echo "gfpull BRANCH"
        return
    fi
    git fetch
    git reset --hard origin/$1
}

gfbranch() {
    if [[ -z "$1" ]] ; then
        echo "git checkout -b, forcely"
        echo "gfbranch BRANCH"
        return
    fi
    git branch -D $1
    git checkout -b $1
}

export GIT_WORKTREE_PREFIX="wrktr-"

gwadd() {
    if [[ -z "$1" ]] ; then
        echo "git worktree add, prefix is ${GIT_WORKTREE_PREFIX}"
        echo "gwadd BRANCH [OPTION]"
        return
    fi
    git worktree add "${GIT_WORKTREE_PREFIX}${1}" "$@"
}

alias gwrm='git worktree remove'
alias gwls='git worktree list'
