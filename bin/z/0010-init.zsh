#!/bin/zsh

# initial settings
ulimit -n 65535
export LOGD=$TMPD/logs
export EDITOR='lmacs'
bindkey -e # emacs
bindkey -r '^T' # disable C-T
setopt print_eight_bit # display Japanese file name
setopt no_beep
unsetopt BEEP
setopt ignore_eof # C-d does not exit zsh
setopt interactive_comments # after '#' is also a comment in command line
# history
export HISTSIZE=10000
export HISTFILE=~/.history_zsh # avoid the tragedy that zsh loads histories
export SAVEHIST=1000000
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
setopt share_history
# completion
autoload -Uz compinit && compinit
autoload -Uz bashcompinit && bashcompinit
setopt correct
zstyle ':completion:*' menu select
# aliases
alias_shortest() {
    alias | rg '^.=' | tr '=' '\t' | sort
}

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
alias l='ls -la'
alias t='cat'
alias y='echo'
alias v='vim'
alias f='less'
alias fn='less -N'
if type eza >/dev/null 2>&1 ; then
    alias ls='eza'
    export EXA_COLORS="da=01:37" # date, white
fi
if type hexyl >/dev/null 2>&1 ; then
    alias od='hexyl'
fi
if type zoxide >/dev/null 2>&1 ; then
    eval "$(zoxide init zsh)"
fi
if type fzf >/dev/null 2>&1 ; then
    alias ff='${DOTFILES_ROOT}/bin/fzf.sh'
fi

export DIRENV_ENABLED=0
direnv-on() {
    eval "$(direnv hook zsh)"
    export DIRENV_ENABLED=1
}

direnv-off() {
    unset -f _direnv_hook
    export DIRENV_ENABLED=0
}

direnv-toggle() {
    if [[ "$DIRENV_ENABLED" = 0 ]] ; then
        direnv-on
    else
        direnv-off
    fi
}

direnv-on
