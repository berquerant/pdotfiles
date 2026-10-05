#!/bin/bash
# emacs-doctor: Diagnostic scanner for Emacs configuration, keybindings, and external tools

set -u

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LIB_PATH="$(cd "${SCRIPT_DIR}/../../common" 2>/dev/null && pwd)/doctor-lib.sh"
if [[ -f "$LIB_PATH" ]]; then
  # shellcheck source=../../common/doctor-lib.sh
  source "$LIB_PATH"
else
  echo "Error: doctor-lib.sh not found at ${LIB_PATH}" >&2
  exit 1
fi

# 1. Environment & Binaries
log_section "1. Emacs Binaries & Dictionaries"
if command -v emacs >/dev/null 2>&1; then
  emacs_ver="$(emacs --version | head -n 1)"
  log_pass "Emacs executable found: ${emacs_ver}"
else
  log_fail "No emacs command found in PATH"
fi

if [[ -d "/Applications/Emacs-GUI.app" ]]; then
  log_pass "Emacs-GUI.app found at /Applications/Emacs-GUI.app"
else
  log_warn "Emacs-GUI.app not found at /Applications/Emacs-GUI.app"
fi

if command -v brew >/dev/null 2>&1; then
  cmigemo_prefix="$(brew --prefix cmigemo 2>/dev/null || true)"
  if [[ -n "$cmigemo_prefix" && -f "${cmigemo_prefix}/share/migemo/utf-8/migemo-dict" ]]; then
    log_pass "Migemo dictionary found"
  else
    log_warn "Migemo dictionary not found at ${cmigemo_prefix}/share/migemo/utf-8/migemo-dict"
  fi
fi

# 2. Syntax & Early-init verification
log_section "2. Elisp Syntax & Early Init Validation"
if command -v emacs >/dev/null 2>&1; then
  if emacs --batch --quick --load .emacs.d/early-init.el >/dev/null 2>&1; then
    log_pass "early-init.el loaded cleanly in batch mode"
  else
    log_fail "early-init.el threw an error in batch mode"
  fi
fi

# 3. Keybinding Conflicts
log_section "3. Keybinding Conflicts"
if [[ -f bin/emacs-key-conflict.sh ]]; then
  conflicts="$(bash bin/emacs-key-conflict.sh r 2>/dev/null || true)"
  if [[ -z "$conflicts" ]]; then
    log_pass "No duplicate keybindings found"
  else
    conflict_count="$(echo "$conflicts" | grep -c -v '^[[:space:]]*$')"
    log_warn "Found ${conflict_count} conflicting key sequences (run: bin/emacs-key-conflict.sh r)"
  fi
else
  log_warn "bin/emacs-key-conflict.sh not found"
fi

# 4. Reformatter Tools in PATH
log_section "4. External Formatter Programs in PATH"
if [[ -f .emacs.d/site-lisp/my-reformatter.el ]]; then
  programs="$(grep -E '^[[:space:]]*:program' .emacs.d/site-lisp/my-reformatter.el | awk -F'"' '{print $2}' | sort -u)"
  for prog in $programs; do
    if command -v "$prog" >/dev/null 2>&1; then
      log_pass "Formatter available in PATH: ${prog}"
    elif [[ -f "bin/${prog}.sh" || -f "bin/${prog}" ]]; then
      log_pass "Formatter available in dotfiles bin: ${prog}"
    else
      log_warn "Formatter NOT found in PATH: ${prog}"
    fi
  done
fi

# Summary
print_summary
