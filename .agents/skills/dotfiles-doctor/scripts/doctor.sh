#!/bin/bash
# dotfiles-doctor: Automated health and integrity scanner for dotfiles

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

# 1. Repository Linters Check
log_section "1. Repository Linters"
if [[ -f .github/bin/lint.sh ]]; then
  if bash .github/bin/lint.sh >/dev/null 2>&1; then
    log_pass "File length limit check (.github/bin/lint.sh)"
  else
    log_fail "File length limit exceeded (.github/bin/lint.sh)"
  fi
fi

if command -v shellcheck >/dev/null 2>&1; then
  if [[ -f .github/bin/shellcheck.sh ]]; then
    if bash .github/bin/shellcheck.sh >/dev/null 2>&1; then
      log_pass "Shellcheck passed"
    else
      log_fail "Shellcheck reported issues (.github/bin/shellcheck.sh)"
    fi
  fi
else
  log_warn "shellcheck not installed, skipping shellcheck"
fi

# 2. Renovate Sync Check
log_section "2. Renovate Configuration Sync"
if command -v yq >/dev/null 2>&1; then
  if [[ -f renovate.yml && -f renovate.json ]]; then
    tmp_json="$(mktemp)"
    yq -o json renovate.yml > "${tmp_json}" 2>/dev/null
    if diff -q renovate.json "${tmp_json}" >/dev/null 2>&1; then
      log_pass "renovate.json is in sync with renovate.yml"
    else
      log_warn "renovate.json differs from renovate.yml (run: yq -o json renovate.yml > renovate.json)"
    fi
    rm -f "${tmp_json}"
  fi
else
  log_warn "yq not installed, skipping renovate.json sync check"
fi

# 3. Requirements Usage Audit
log_section "3. Requirements Usage Audit"

audit_requirements_file() {
  local req_file="$1"
  local type="$2"
  if [[ ! -f "$req_file" ]]; then
    return
  fi

  echo "  Auditing ${req_file}..."
  while IFS= read -r line || [[ -n "$line" ]]; do
    [[ -z "$line" || "$line" =~ ^[[:space:]]*# ]] && continue

    local pkg=""
    local search_terms=()
    case "$type" in
      "cargo"|"node")
        pkg="${line%%@*}"
        search_terms=("$pkg")
        if [[ "$pkg" == "evcxr_repl" ]]; then search_terms+=("evcxr"); fi
        if [[ "$pkg" == "typescript-language-server" ]]; then search_terms+=("typescript-language-server" "typescript"); fi
        ;;
      "gem")
        pkg="$(echo "$line" | awk '{print $1}')"
        search_terms=("$pkg")
        ;;
      "go")
        pkg="$(echo "$line" | sed -E 's/@.*$//' | awk -F'/' '{print $NF}')"
        search_terms=("$pkg")
        ;;
      "python")
        pkg="${line%%=*}"
        search_terms=("$pkg")
        if [[ "$pkg" == "pyyaml" ]]; then search_terms+=("yaml"); fi
        ;;
      "rustup")
        pkg="$line"
        if [[ "$pkg" == "rust-src" || "$pkg" == "rust-analyzer" ]]; then
          # rust-src is standard for rust-analyzer
          continue
        fi
        search_terms=("$pkg")
        ;;
    esac

    [[ -z "$pkg" ]] && continue

    local found=0
    for term in "${search_terms[@]}"; do
      local count
      count="$(git grep -i -F "$term" -- ':!.github' ':!requirements' ':!renovate.*' ':!ivg/locks' ':!ivg/renovate.lock' 2>/dev/null | wc -l | tr -d ' ')"
      if [[ "$count" -gt 0 ]]; then
        found=1
        break
      fi
    done

    if [[ "$found" -eq 0 ]]; then
      log_warn "Unreferenced package in ${req_file}: ${pkg}"
    fi
  done < "$req_file"
}

audit_requirements_file "requirements/cargo" "cargo"
audit_requirements_file "requirements/gem" "gem"
audit_requirements_file "requirements/go" "go"
audit_requirements_file "requirements/node" "node"
audit_requirements_file "requirements/python" "python"
audit_requirements_file "requirements/rustup" "rustup"

# 4. Reformatter Program Availability
log_section "4. Emacs Reformatter Programs"
if [[ -f .emacs.d/site-lisp/my-reformatter.el ]]; then
  programs="$(grep -E '^[[:space:]]*:program' .emacs.d/site-lisp/my-reformatter.el | awk -F'"' '{print $2}' | sort -u)"
  for prog in $programs; do
    if git grep -q -F "$prog" requirements/ .Brewfile 2>/dev/null || [[ -f "bin/${prog}.sh" ]]; then
      log_pass "Reformatter program tracked: ${prog}"
    else
      log_warn "Reformatter program configured but NOT tracked in requirements/.Brewfile/bin: ${prog}"
    fi
  done
fi

# 5. Homebrew Health
log_section "5. Homebrew Health (brew doctor)"
if command -v brew >/dev/null 2>&1; then
  echo "  Running brew doctor..."
  if brew doctor >/dev/null 2>&1; then
    log_pass "brew doctor: your system is ready to brew"
  else
    log_warn "brew doctor reported warnings (run 'brew doctor' to inspect)"
  fi
else
  log_warn "brew is not installed, skipping brew doctor"
fi

# 6. Specialized Sub-Doctors (when --all or -a is passed)
if [[ "${1:-}" == "--all" || "${1:-}" == "-a" ]]; then
  log_section "6. Specialized Sub-Doctors (--all)"

  subdoctors=(
    "IVG Doctor:${SCRIPT_DIR}/../../ivg-workflow/scripts/ivg-doctor.sh"
    "Renovate Sync:${SCRIPT_DIR}/../../renovate-ops/scripts/renovate-sync.sh"
    "Emacs Doctor:${SCRIPT_DIR}/../../emacs-healthcheck/scripts/emacs-doctor.sh"
  )

  for entry in "${subdoctors[@]}"; do
    name="${entry%%:*}"
    sub_script="${entry##*:}"

    if [[ -f "$sub_script" ]]; then
      echo -e "\n--- [Sub-Doctor: ${name}] ---"
      if bash "$sub_script"; then
        log_pass "Sub-doctor completed: ${name}"
      else
        log_fail "Sub-doctor reported failures: ${name}"
      fi
    else
      log_warn "Sub-doctor not found: ${sub_script}"
    fi
  done
fi

# Summary
print_summary
