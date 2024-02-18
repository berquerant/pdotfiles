#!/bin/bash


find "$EMACSD" -name "*.elc" -delete
rm -rf "${EMACSD}/eln-cache" "${EMACSD}/.cache"
find "$EMACSD" -name "*.eln" -delete
