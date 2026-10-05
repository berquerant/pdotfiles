#!/bin/bash
# renovate-sync: Synchronize renovate.yml to renovate.json and validate regex matchers

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

# 1. Compile renovate.yml to renovate.json
log_section "1. Synchronize renovate.yml to renovate.json"
if ! command -v yq >/dev/null 2>&1; then
  log_fail "yq is not installed. Cannot compile renovate.yml"
else
  tmp_json="$(mktemp)"
  if yq -o json renovate.yml > "${tmp_json}" 2>/dev/null; then
    if [[ -f renovate.json ]] && diff -q renovate.json "${tmp_json}" >/dev/null 2>&1; then
      log_pass "renovate.json is already up to date"
    else
      cp -f "${tmp_json}" renovate.json
      log_pass "Compiled renovate.yml into renovate.json successfully"
    fi
  else
    log_fail "Failed to parse renovate.yml with yq"
  fi
  rm -f "${tmp_json}"
fi

# 2. Test target file matching for custom managers (dynamically parsed from renovate.json)
log_section "2. Verify Custom Manager Targets (Dynamic)"
if ! command -v jq >/dev/null 2>&1; then
  log_warn "jq is not installed, skipping dynamic customManager validation"
elif [[ ! -f renovate.json ]]; then
  log_warn "renovate.json not found, skipping dynamic customManager validation"
else
  while IFS= read -r mgr || [[ -n "$mgr" ]]; do
    [[ -z "$mgr" ]] && continue
    desc="$(echo "$mgr" | jq -r '.description // "customManager"')"
    files="$(echo "$mgr" | jq -r 'if (.fileMatch | type) == "array" then .fileMatch[] else .fileMatch end' | sed -E 's/^\^//; s/\$$//')"
    regex="$(echo "$mgr" | jq -r '.matchStrings[0]')"

    for f in $files; do
      if [[ ! -f "$f" ]]; then
        log_warn "Target file not found: ${f} (${desc})"
        continue
      fi

      if perl -e 'my ($re, $path) = @ARGV; open my $fh, "<", $path or exit 2; while (<$fh>) { if (/$re/) { exit 0; } } exit 1;' "$regex" "$f" 2>/dev/null; then
        log_pass "Regex match confirmed: ${f} (${desc})"
      else
        log_fail "Regex failed to match: ${f} (${desc})"
      fi
    done
  done < <(jq -c '.customManagers[]' renovate.json 2>/dev/null)
fi

# 3. Renovate official config validator (if docker running)
log_section "3. Official Config Validator"
if command -v docker >/dev/null 2>&1 && docker info >/dev/null 2>&1; then
  echo "  Running official renovate-config-validator via Docker..."
  if bash bin/renovate-validate.sh >/dev/null 2>&1; then
    log_pass "renovate-config-validator --strict passed"
  else
    log_fail "renovate-config-validator --strict failed"
  fi
else
  log_warn "Docker is not available/running; skipping containerized validator"
fi

# Summary
print_summary
