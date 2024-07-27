#!/bin/zsh

export HOMEBREW_BUNDLE_FILE_GLOBAL="$HOME/.brewfile"

alias arm="exec arch -arch arm64e /bin/zsh --login"
alias x64="exec arch -arch x86_64 /bin/zsh --login"

"${DOTFILES_ROOT}/bin/brew-eval.sh"
