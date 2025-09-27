#!/bin/zsh

export MIGEMO_DICT="$(brew --prefix cmigemo)/share/migemo/utf-8/migemo-dict"
export EMACS_HISTFILE="${EMACSD}/.history_emacs"
export EMACS_CUI="/usr/local/bin/emacs"
export EMACS_GUI="/Applications/Emacs-GUI.app"
# for emacs-light.sh
export EMACS_MINIMAL_INIT="${EMACSD}/straight/repos/emacs-minimal-init"
# for my-open-file.el, emacs-open.sh
export EMACS_OPEN_FILE_TARGET="${TMPD}/.emacs-open-file-target"

emacs_gui() {
    if [[ -z "$1" ]] ; then
        open -a "$EMACS_GUI"
    else
        open -a "$EMACS_GUI" --args "$@"
    fi
}

etoggle() {
    EMACSD="$CMACSD" "$@"
}

emacs_cui() {
    etoggle "$EMACS_CUI" --init-directory="$CMACSD" "$@"
}

emacs_docker() {
    "${DOTFILES_ROOT}/bin/docker.sh" run -it "$@" docker-debian-emacs
}

alias emacs="emacs_gui"
alias gmacs="emacs_gui"
alias cmacs="emacs_cui"
alias dmacs="emacs_docker"
alias e='lmacs'
alias o='${DOTFILES_ROOT}/bin/emacs-open.sh'
alias emacs_keys='${DOTFILES_ROOT}/bin/emacs-key-conflict.sh'

kill_emacs() {
    pkill -KILL "[eE]macs"
}
alias ekill='kill_emacs'

emacs_batch() {
    etoggle "${DOTFILES_ROOT}/bin/emacs-batch.sh" "$@"
}

emacs_package() {
    etoggle "${DOTFILES_ROOT}/bin/emacs-package.sh" "$@"
}

# vterm https://github.com/akermu/emacs-libvterm
vterm_printf() {
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ]); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

if [[ "$INSIDE_EMACS" = 'vterm' ]] \
    && [[ -n ${EMACS_VTERM_PATH} ]] \
    && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh ]]; then
    TMP_PROMPT_COMMAND=$PROMPT_COMMAND
    TMP_PS1=$PS1
    source ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh
    PS1=$TMP_PS1
    PROMPT_COMMAND=$TMP_PROMPT_COMMAND
fi
