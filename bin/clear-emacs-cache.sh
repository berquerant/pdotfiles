#!/bin/bash


find "$EMACSD" -name "*.elc"
rm -rf "${EMACSD}/eln-cache" "${EMACSD}/.cache"
find "$EMACSD" -name "*.eln"
