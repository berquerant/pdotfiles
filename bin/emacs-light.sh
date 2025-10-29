#!/bin/bash

EMACS="${EMACS_CUI:-emacs}"

emacs() {
    "$EMACS" --no-loadup \
             --no-window-system \
             --quick \
             "$@"
}

if [ -n "$EMACS_MINIMAL_INIT" ] ; then
    # https://github.com/berquerant/emacs-minimal-init
    emacs --load "${EMACS_MINIMAL_INIT}/minimal-init.el" \
          --eval "(setq minimal-init-quiet t)" \
          --funcall minimal-init-setup \
          "$@" < /dev/tty
else
    emacs "$@" < /dev/tty
fi
