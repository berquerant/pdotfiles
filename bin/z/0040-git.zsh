#!/bin/zsh

export GHQ_ROOT=$HOME/src
export GIT_USER="$(git config user.name)"
alias gdpull='git default-pull'
alias gclean='git delete-merged'
alias glis='${DOTFILES_ROOT}/bin/git-ls.sh'
alias r='repo'
alias glint='${DOTFILES_ROOT}/bin/ghalint.sh'
alias clint='${DOTFILES_ROOT}/bin/codelint.sh'

repo() {
  location="$($DOTFILES_ROOT/bin/git-get.sh $@)"
  if [ -z "$location" ]; then
    return 1
  fi
  cd "$location"
}

export GIT_ITER_REPOS_ROOT="$GHQ_ROOT"
export GIT_ITER_WORKTREE_ROOT="${GHQ_ROOT}/git-worktree"
alias gi='git-iter'
alias gwt='gi wt'
alias rwt='repowt'

repowt() {
  location="$(git-iter wt ls --all | ${DOTFILES_ROOT}/bin/fzf.sh)"
  if [ -z "$location" ]; then
    return 1
  fi
  cd "$location"
}

gprune() {
  git switch "$(git default-branch)" &&\
    gwt prune &&\
    gclean
}
