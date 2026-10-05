#!/bin/bash
# ivg-doctor: Integrity check for IVG (Install Via Git) configs and lockfiles

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

# 1. Inspect targets lists
log_section "1. Target List Definitions"
for target_file in targets/*; do
  [[ ! -f "$target_file" ]] && continue
  cat_name="$(basename "$target_file")"
  echo "  Checking targets in ${cat_name}..."
  while IFS= read -r target || [[ -n "$target" ]]; do
    [[ -z "$target" || "$target" =~ ^[[:space:]]*# ]] && continue
    yml_file="ivg/${target}.yml"
    if [[ -f "$yml_file" ]]; then
      log_pass "Found definition: ${target} -> ${yml_file}"
    else
      log_fail "Missing definition for target in ${cat_name}: ${target} (${yml_file} not found)"
    fi
  done < "$target_file"
done

# 2. Inspect IVG YAML configs & lockfiles
log_section "2. IVG YAML Configs and Locks"
for yml in ivg/*.yml; do
  [[ ! -f "$yml" ]] && continue
  target_name="$(basename "$yml" .yml)"

  # YAML syntax validation
  if command -v yq >/dev/null 2>&1; then
    if yq '.' "$yml" >/dev/null 2>&1; then
      log_pass "Valid YAML syntax: ${yml}"
    else
      log_fail "Syntax error in YAML: ${yml}"
      continue
    fi
  fi

  # Check lock file location
  lock_path="$(grep -E '^[[:space:]]*lock:' "$yml" | awk '{print $2}')"
  if [[ -n "$lock_path" ]]; then
    full_lock="ivg/${lock_path}"
    if [[ -f "$full_lock" ]]; then
      log_pass "Lock file exists for ${target_name}: ${full_lock}"
    else
      log_warn "Missing lock file for ${target_name}: ${full_lock}"
    fi
  else
    log_warn "No lock property declared in: ${yml}"
  fi

  # Check if target is mapped in targets/ or README.md
  if git grep -q -F "$target_name" targets/ README.md 2>/dev/null; then
    log_pass "Target ${target_name} is referenced in targets/ or README.md"
  else
    log_warn "Target ${target_name} is not referenced in targets/ or README.md"
  fi
done

# 3. Renovate lock integrity
log_section "3. Renovate Lockfile"
if [[ -f "ivg/renovate.lock" ]]; then
  if [[ -s "ivg/renovate.lock" ]]; then
    entry_count="$(wc -l < "ivg/renovate.lock" | tr -d ' ')"
    log_pass "ivg/renovate.lock exists with ${entry_count} entries"
  else
    log_warn "ivg/renovate.lock is empty"
  fi
else
  log_warn "ivg/renovate.lock is missing (run: bin/renovate-ivg.sh gen)"
fi

# Summary
print_summary
