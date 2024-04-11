#!/bin/bash

/usr/local/bin/emacs --batch --quick --load "${EMACSD}/init.el" "$@" 2> /dev/null
