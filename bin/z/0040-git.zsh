#!/bin/zsh

export GIT_EDITOR='lmacs'
export GHQ_ROOT=$HOME/src
export GIT_USER=`git config user.name`
alias g='git'
alias a='g a'
alias b='g b'
alias c='g c'
alias d='g d'
alias m='g cm'
alias s='g s'
alias r='repo'
alias gg='g g'
alias gdefault='${DOTFILES_ROOT}/bin/default-branch.sh'
alias gdpull='${DOTFILES_ROOT}/bin/default-branch.sh pull true false'
alias gpullback='${DOTFILES_ROOT}/bin/default-branch.sh pull false true'
alias gfbranch='${DOTFILES_ROOT}/bin/default-branch.sh branch'
alias gworktree='${DOTFILES_ROOT}/bin/git-worktree.sh'

repo() {
    location="$($DOTFILES_ROOT/bin/git-get.sh $@)"
    if [ -z "$location" ] ; then
        return 1
    fi
    cd "$location"
}

gi() {
    GIT_ITER_MAX_PROCS="${GI_PROCS:-1}" GREP='rg' git-iter "$@"
}

groot() {
    cd "$(git root)"
}
