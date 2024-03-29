#!/bin/zsh

peco-history-selection() {
    BUFFER=`history -n 1 | tail -r | awk '!a[$0]++' | peco | cut -b22-`
    CURSOR=$#BUFFER
    zle reset-prompt
}

zle -N peco-history-selection
bindkey '^r' peco-history-selection

peco-compgen-selection() {
    BUFFER=`compgen -abck | sort | uniq | awk '{print length(), $0}' | sort -n | awk '{print $2}' | peco`
    CURSOR=$#BUFFER
    zle reset-prompt
}
