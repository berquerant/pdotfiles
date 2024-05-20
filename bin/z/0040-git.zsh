#!/bin/zsh

export GIT_EDITOR='vim'
export GHQ_ROOT=$HOME/go/src
export GIT_USER=`git config user.name`
alias g='git'
alias gg='g g'
alias d='g d'
alias a='g a'
alias c='g c'
alias m='g cm'
alias o='gdefaultswitch'
alias s='g s'
alias r='repo'
alias v='gdefaultdiff'

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

gfbranch() {
    if [[ -z "$1" ]] ; then
        echo "switch branch if remote branch exists else create branch, forcely"
        echo "gfbranch BRANCH"
        return 1
    fi

    branch="$1"
    if git branch | grep -q "$branch" ; then
        git branch -D "$branch"
    fi
    git fetch
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
    "${DOTFILES_ROOT}/bin/git-iter.sh" "$@"
}
