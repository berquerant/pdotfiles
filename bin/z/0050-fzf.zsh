#!/bin/zsh

fzf-history-selection() {
    BUFFER=`history -n 1 | tail -r | awk '!a[$0]++' | "${DOTFILES_ROOT}/bin/fzf.sh" | cut -b22-`
    CURSOR=$#BUFFER
    zle reset-prompt
}

zle -N fzf-history-selection
bindkey '^r' fzf-history-selection

fzf-compgen-selection() {
    BUFFER=`compgen -abck | sort | uniq | awk '{print length(), $0}' | sort -n | awk '{print $2}' | "${DOTFILES_ROOT}/bin/fzf.sh"`
    CURSOR=$#BUFFER
    zle reset-prompt
}
