#!/bin/zsh

export GHQ_ROOT=$HOME/src
export GIT_USER="$(git config user.name)"
alias gdefault='${DOTFILES_ROOT}/bin/default-branch.sh'
alias gdpull='${DOTFILES_ROOT}/bin/default-branch.sh pull true false'
alias gpullback='${DOTFILES_ROOT}/bin/default-branch.sh pull false true'
alias gfbranch='${DOTFILES_ROOT}/bin/default-branch.sh branch'
alias gworktree='${DOTFILES_ROOT}/bin/git-worktree.sh'
alias glis='${DOTFILES_ROOT}/bin/git-ls.sh'
alias glis-gb='glis gbrowse'
alias glis-t='glis cat'
alias glis-f='glis less'
alias glis-fn='gliss less -N'
alias glis-o='glis ${DOTFILES_ROOT}/bin/emacs-open.sh'
alias glis-e='glis lmacs'
alias glis-u='glis-t | umacs'
alias r='repo'

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
