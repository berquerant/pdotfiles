#!/bin/zsh

# initial settings
ulimit -n 65535
export TMPD=$HOME/tmp
export LOGD=$TMPD/logs
export EDITOR='vim'
bindkey -e # emacs
bindkey -r '^T' # disable C-T
setopt print_eight_bit # display Japanese file name
setopt no_beep
unsetopt BEEP
setopt ignore_eof # C-d does not exit zsh
setopt interactive_comments # after '#' is also a comment in command line
# history
export HISTSIZE=1000
export HISTFILE=~/.history_zsh # avoid the tragedy that zsh loads histories
export SAVEHIST=100000
export LESSHISTFILE=- # no histories for less
setopt hist_ignore_dups
setopt hist_ignore_all_dups
setopt hist_no_store # ignore 'history' command
setopt hist_verify
setopt hist_expand
setopt hist_reduce_blanks
setopt extended_history
setopt inc_append_history
setopt no_tify
# completion
autoload -Uz compinit && compinit
autoload -Uz bashcompinit && bashcompinit
setopt correct
zstyle ':completion:*' menu select
# aliases
alias -s sh=sh
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
alias pc='pbcopy'
alias pp='pbpaste'
alias watch='watch -cd'
alias less='less -R'
alias ldd='otool -L'
alias dc='cd'
alias history='history -t "%F %T"'
if type exa >/dev/null 2>&1 ; then
    export EXA_COLORS="da=01:37" # Date Modified into white
    alias ls='exa'
fi
if type hexyl >/dev/null 2>&1 ; then
    alias od='hexyl'
fi
