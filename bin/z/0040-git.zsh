#!/bin/zsh

export GHQ_ROOT=$HOME/src
export GIT_USER="$(git config user.name)"
alias gdefault='${DOTFILES_ROOT}/bin/default-branch.sh'
alias gdpull='${DOTFILES_ROOT}/bin/default-branch.sh pull true false'
alias gclean='${DOTFILES_ROOT}/bin/default-branch.sh cleanup true'
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

gi() {
  git-iter "$@"
}
