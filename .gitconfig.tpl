[user]
  name = ${GIT_USER_NAME}
  email = ${GIT_USER_EMAIL}
[init]
  defaultBranch = main
[grep]
  lineNumber = true
  extendedRegexp = true
  threads = 4
[color]
  ui = auto
[diff]
  renames = true
  tool = vimdiff
  compactionHeuristic = true
[difftool]
  prompt = false
[core]
  editor = lmacs
  excludesfile = ~/.gitignore
  ignorecase = false
  quotepath = false
  autocrlf = input
  safecrlf = false
  commentChar = ";"
[log]
  abbrevCommit = true
[fetch]
  prune = true
[pull]
  ff = only
[http]
  postBuffer = 104857600
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
  ds = diff --name-status
  merged = branch --merged
  delete-merged = !git branch --merged | grep -vE '(\\*|main|master)' | xargs git branch -d
  force-push = push --force-with-lease --force-if-includes
  g = grep --heading
  l = log
  ll = log --pretty=oneline
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
  p = pull
  w = worktree
  rename-branch = branch -m
  fix-comment = commit --amend
  which-branch = branch --contains
  default-branch = !git remote show origin | grep -F 'HEAD branch:' | cut -d ':' -f 2 | tr -d ' '
  force-remove-untracked = clean -d -f
  clone-latest-only = clone --depth 1
  tag-push = !"f(){ if [ -z \"$1\" ]; then echo 'Usage: git tag-push TAG'; return 1; fi; git tag \"$1\" && git push origin \"$1\"; }; f"
  tag-delete = !"f(){ if [ -z \"$1\" ]; then echo 'Usage: git tag-delete TAG'; return 1; fi; git tag -d \"$1\" && git push -d origin \"$1\"; }; f"
  tag-latest = describe --abbrev=0 --tags
  tag-list = tag -l --sort=creatordate --format='%(creatordate:unix) %(creatordate:iso-strict) %(refname)'
  tag-neighbor = !"f(){ if [ -z \"$1\" ]; then echo 'Usage: git tag-next TAG'; return 1; fi; git tag-list | grep -E \"refs/tags/${1}$\" -C 1; }; f"
  current-branch = "!git branch --contains | awk '$1==\"*\"{print $2}'"
  default-branch = "!git remote show origin | grep -F 'HEAD branch:' | cut -d ':' -f 2 | tr -d ' '"
  force-reset = !"f(){ if [ -z \"$1\" ]; then echo 'Usage: git force-reset BRANCH'; return 1; fi; git fetch && git reset --hard \"origin/$1\"; }; f"
