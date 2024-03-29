#!/bin/zsh

export MIGEMO_DICT="$(brew --prefix cmigemo)/share/migemo/utf-8/migemo-dict"
export EMACS_HISTFILE=$EMACSD/.history_emacs

emacs_gui() {
    emacs_app="/Applications/Emacs-GUI.app"
    if [[ -z "$1" ]] ; then
        open -a "$emacs_app"
    else
        open -a "$emacs_app" --args "$@"
    fi
}

emacs_cui() {
    /usr/local/bin/emacs "$@"
}

emacs_docker() {
    "${DOTFILES_ROOT}/bin/docker-rmit.sh" "$@" docker-debian-emacs
}

alias emacs="emacs_gui"
alias gmacs="emacs_gui"
alias cmacs="emacs_cui"
alias dmacs="emacs_docker"

kill_emacs() {
    pkill -KILL "[eE]macs"
}
alias ekill='kill_emacs'

if [[ "$INSIDE_EMACS" = 'vterm' ]] \
    && [[ -n ${EMACS_VTERM_PATH} ]] \
    && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh ]]; then
    TMP_PROMPT_COMMAND=$PROMPT_COMMAND
    TMP_PS1=$PS1
    source ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh
    PS1=$TMP_PS1
    PROMPT_COMMAND=$TMP_PROMPT_COMMAND
fi

emacs_batch() {
    "${DOTFILES_ROOT}/bin/emacs-batch.sh" "$@"
}

emacs_package() {
    "${DOTFILES_ROOT}/bin/emacs-package.sh" "$@"
}

emacs_select_update_package() {
    emacs_package ls name | peco | while read name ; do
        emacs_package update "$name"
    done
}

# ld: library not found for -lemutls_w
# libgccjit.so: error: error invoking gcc driver
export LIBRARY_PATH="${LIBRARY_PATH}:$(brew --prefix gcc)/lib/gcc/current:$(brew --prefix libgccjit)/lib/gcc/current:$(brew --prefix gcc)/lib/gcc/current/gcc/aarch64-apple-darwin23/13"
