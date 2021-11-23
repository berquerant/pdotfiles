[user]
  name = ${GIT_USER_NAME}
  email = ${GIT_USER_EMAIL}
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
  attributesfile = ~/.gitattributes
  excludesfile = ~/.gitignore
  ignorecase = false
[log]
  abbrevCommit = true
[instaweb]
  local = true
  httpd = python
  port = 39995
[alias]
  a = add
  ai = add -i
  aliases = !git config -l | grep -E "^alias" | cut -d "." -f 2- | sort
  b = branch
  bm = branch -m
  bd = branch -d
  bl = blame
  bs = bisect
  c = checkout
  config-list = config --list
  clone-shallow = clone --depth 1
  cm = commit
  cma = commit --amend
  cobj = count-objects --human-readable
  d = diff
  ds = diff --stat
  dt = difftool
  dw = diff --color-words
  delete-merged-branch = !git branch --merged | grep -v master | xargs git branch -d
  f = fetch
  find-branch = branch --contains
  g = grep --heading
  head-hash = !sh -c 'git rev-list $1 | head -n 1' -
  tail-hash = !sh -c 'git rev-list --reverse $1 | head -n 1' -
  head-branch = !sh -c 'git rev-parse --abbrev-ref $1' -
  human = name-rev --name-only --refs=refs/heads/*
  iw = instaweb
  l = log
  logs = log -S --pickaxe-regex
  lg = log -G
  lp = log -p
  lf = log --follow
  ls = ls-files -t
  ls-excluded = ls-files --others -i --exclude-standard
  ls-other = ls-files --others
  lstat = log --stat
  rl = reflog
  rp = rev-parse
  rv = revert
  root = rev-parse --show-toplevel
  s = status --short --branch
  secrets-add = secrets --add
  secrets-allow = secrets --add --allowed --literal
  secrets-scan = secrets --scan
  secrets-hist = secrets --scan-history
  secrets-init = !git secrets --install && git secrets --register-aws
  secrets-list = secrets --list
  secrets-unset = config --unset secrets.providers
  w = show
  st = stash
  su = submodule update --init --recursive
  uc = reset --soft @^
  us = reset @
