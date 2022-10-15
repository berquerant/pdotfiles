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
[alias]
  a = add
  aliases = !git config --list | grep -E "^alias" | cut -d "." -f 2- | sort
  b = branch
  c = checkout
  clone-shallow = clone --depth 1
  cm = commit
  count-obj = count-objects --human-readable
  d = diff
  dt = difftool
  dw = diff --color-words
  merged = git branch --merged
  g = grep --heading
  l = log
  logs = log --graph --all --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative
  ls = ls-files
  rl = reflog
  root = rev-parse --show-toplevel
  s = status --short --branch
  w = show
  undo-commit = reset --soft @^
  undo-add = reset @
  recent = for-each-ref refs/heads/ --sort=-committerdate --format='%(committerdate:iso) %(color:green)%(refname:short)%(color:reset) %(color:red)%(authoremail) %(authorname)%(color:reset) %(subject)'
