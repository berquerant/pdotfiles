[user]
  name = ${GIT_USER_NAME}
  email = ${GIT_USER_EMAIL}
[init]
  defaultBranch = main
[grep]
  lineNumber = true
  extendedRegexp = true
  threads = 2
[color]
  ui = auto
[diff]
  renames = true
  tool = vimdiff
  compactionHeuristic = true
[difftool]
  prompt = false
[core]
  editor = vim
  excludesfile = ~/.gitignore
  ignorecase = false
  quotepath = false
  autocrlf = input
  safecrlf = false
[log]
  abbrevCommit = true
[fetch]
  prune = true
[pull]
  ff = only
[alias]
  a = add
  aliases = !git config --list | grep -E '^alias' | cut -d '.' -f 2- | sort
  b = branch
  c = checkout
  clone-shallow = clone --depth 1
  cm = commit
  count-obj = count-objects --human-readable
  d = diff
  dt = difftool
  dw = diff --color-words
  merged = branch --merged
  g = grep --heading
  l = log
  logs = log --graph --all --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative
  search-commit = log -G
  search-commit-patch = log --patch -G
  search-commit-patch-first = log --pickaxe-regex --patch -S
  search-commit-message = log --grep
  search-branch = name-rev --refs="refs/heads/*"
  ls = ls-files
  rl = reflog
  root = rev-parse --show-toplevel
  s = status --short --branch
  undo-commit = reset --soft @^
  undo-add = reset @
  recent = for-each-ref refs/heads/ --sort=-committerdate --format='%(committerdate:iso) %(color:green)%(refname:short)%(color:reset) %(color:red)%(authoremail) %(authorname)%(color:reset) %(subject)'
  redo-commit = revert @
  hash = show --format='%H' --no-patch
  log-num = log --numstat
  log-name = log --name-status
  ps = push origin HEAD
  w = worktree
  rename-branch = branch -m
  fix-comment = commit --ammend
  which-branch = branch --contains
  default-branch = !git remote show origin | grep -F 'HEAD branch:' | cut -d ':' -f 2 | tr -d ' '
