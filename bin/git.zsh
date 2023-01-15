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
        echo "git_grep_repo GIT_GREP_OPTIONS"
        return
    fi
    repo && git grep "$@"
}
