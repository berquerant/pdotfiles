#!/bin/zsh

export EMACSD=$HOME/.emacs.d
export MIGEMO_DICT="$(brew --prefix cmigemo)/share/migemo/utf-8/migemo-dict"
export EMACS_HISTFILE=$EMACSD/.history_emacs

emacs() {
    emacs_app="/Applications/Emacs.app"
    if [[ -z "$1" ]] ; then
        open -a "$emacs_app"
    else
        open -a "$emacs_app" --args "$@"
    fi
}

kill_emacs() {
    ps aux | rg "[/]Applications/Emacs" | awk '{print $2}' | xargs kill -9
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
