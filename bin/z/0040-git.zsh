#!/bin/zsh

export GIT_EDITOR='lmacs'
export GHQ_ROOT=$HOME/src
export GIT_USER=`git config user.name`
alias g='git'
alias d='g d'
alias a='g a'
alias c='g c'
alias m='g cm'
alias o='gdefaultswitch'
alias s='g s'
alias r='repo'
alias b='gdefaultdiff'
alias p='gdefaultpull'

repo() {
    location="$($DOTFILES_ROOT/bin/git-get.sh $@)"
    if [ -z "$location" ] ; then
        return 1
    fi
    cd "$location"
}

gdefault() {
    "${DOTFILES_ROOT}/bin/default-branch.sh" "$@"
}

gdefaultpull() {
    gdefault pull "$@"
}

gdefaultswitch() {
    gdefault switch
}

gdefaultdiff() {
    gdefault diff
}

gtagpush() {
    if [[ -z "$1" ]] ; then
        echo "create tag and push it"
        echo "gtagpush TAG"
        return 1
    fi
    git tag "$1"
    git push origin "$1"
}

gcurrentbranch() {
    git branch | awk '$1=="*"{print $2}'
}

gfbranch() {
    if [[ "$1" == "-h" ]] ; then
        echo "switch branch if remote branch exists else create branch, forcely"
        echo "if BRANCH is empty, delete the current branch and create a new branch with the same name"
        echo "gfbranch [BRANCH]"
        return 1
    fi

    default_branch="$(gdefault)"
    current_branch="$(gcurrentbranch)"
    branch="${1:-$current_branch}"
    if [[ "$branch" == "$default_branch" ]] ; then
        return 1
    fi
    git switch "$default_branch"
    git branch -D "$branch"
    git fetch
    git pull
    git switch "$branch" || git checkout -b "$branch"
}

gworktree() {
    "${DOTFILES_ROOT}/bin/git-worktree.sh" "$@"
}

gsubremove() {
    if [[ -z "$1" ]] ; then
        echo "remove submodule"
        echo "gsubremove MODULE"
        return 1
    fi
    git submodule deinit -f "$1" && git rm -f "$1" && rm -rf .git/modules/ "$1"
}

gfreset() {
    if [[ -z "$1" ]] ; then
        echo "sync to remote branch"
        echo "gfreset BRANCH"
        return 1
    fi
    git fetch && git reset --hard "origin/$1"
}

gi() {
    GIT_ITER_MAX_PROCS="${GI_PROCS:-1}" GREP='rg' git-iter "$@"
}
