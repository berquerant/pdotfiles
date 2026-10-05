#!/bin/bash
# doctor-lib.sh: Common logging, repository root resolution, and assertions for doctor scripts

set -u

# Resolve repository root robustly
DOTFILES_DIR="$(git rev-parse --show-toplevel 2>/dev/null || true)"
if [[ -z "$DOTFILES_DIR" ]]; then
  DOTFILES_DIR="${DOTFILES_ROOT:-$(pwd)}"
fi
export DOTFILES_DIR
cd "${DOTFILES_DIR}" || exit 1

# Color definitions
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

pass_count=0
warn_count=0
fail_count=0

log_section() {
  echo -e "\n${BLUE}=== $1 ===${NC}"
}

log_pass() {
  echo -e "  ${GREEN}[PASS]${NC} $1"
  ((pass_count++))
}

log_warn() {
  echo -e "  ${YELLOW}[WARN]${NC} $1"
  ((warn_count++))
}

log_fail() {
  echo -e "  ${RED}[FAIL]${NC} $1"
  ((fail_count++))
}

print_summary() {
  echo -e "\n${BLUE}=== Summary ===${NC}"
  echo -e "  Passed:   ${GREEN}${pass_count}${NC}"
  echo -e "  Warnings: ${YELLOW}${warn_count}${NC}"
  echo -e "  Failed:   ${RED}${fail_count}${NC}"

  if [[ "$fail_count" -gt 0 ]]; then
    return 1
  fi
  return 0
}
