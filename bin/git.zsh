#!/bin/zsh

export GIT_EDITOR='vim'
export GHQ_ROOT=$HOME/go/src
alias g='git'
alias glall='git log --graph --all --pretty=format:"%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset" --abbrev-commit --date=relative'

alias repos='ghq list -p | peco'
alias repo='cd $(repos)'
alias repopath='ghq list | peco | cut -d "/" -f 2,3'
alias rrepo='hub browse $(repopath)'
