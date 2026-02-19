#!/bin/zsh

# initial settings
export LOGD=$TMPD/logs
# aliases
alias_shortest() {
    alias | rg '^.=' | tr '=' '\t' | sort
}

alias -s sh=sh
if type eza >/dev/null 2>&1 ; then
    alias ls='eza'
    export EZA_COLORS="da=37;1" # date, white, bold
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
