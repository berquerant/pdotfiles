---
name: dotfiles-doctor
description: >-
  Diagnose dotfiles integrity, verify dependencies, and detect configuration drift.
  Use this skill when checking for unused or missing tools in requirements, verifying
  symlinks, running linters (shellcheck, size limits), or ensuring Renovate configuration
  matches requirement files.
---

# Dotfiles Doctor

This skill provides procedures and automated checks to inspect the integrity and consistency of the dotfiles repository.

## Capabilities

1. **Dependency Drift Detection**: Identifies tools listed in `requirements/` that are not referenced in configs (`.emacs.d/`, `bin/`, `.zshrc`), as well as commands referenced in configs that are missing from `requirements/` or `.Brewfile`.
2. **Symlink & Path Integrity**: Verifies that deployed files and managed symlinks exist.
3. **Repository Lints & Standards**: Runs repository-defined linters (`.github/bin/lint.sh`, Shellcheck, code size threshold).
4. **Renovate Mapping Verification**: Confirms that package versions in `requirements/*` adhere to the matchers in `renovate.yml`.

---

## Diagnostic Workflow

### 1. Run Comprehensive Health Check

Execute the bundled diagnostic script from the repository root:

```bash
# Run basic health check (linters, requirements, reformatter)
bash .agents/skills/dotfiles-doctor/scripts/doctor.sh

# Run full composite diagnostics (orchestrates all specialized sub-doctors: IVG, Renovate, Emacs)
bash .agents/skills/dotfiles-doctor/scripts/doctor.sh --all
```

The script evaluates:
* **Shell & Size Linters**: Line count limits (`.github/bin/lint.sh`) and Shellcheck.
* **Requirements Usage**: Scans `requirements/{cargo,gem,go,node,python,rustup}` against repository configs.
* **Reformatter & Tool Availability**: Cross-checks programs called in `.emacs.d/site-lisp/my-reformatter.el` against package definitions.
* **Homebrew Health**: Runs `brew doctor` to inspect system-wide package and formula readiness.
* **Specialized Sub-Doctors (`--all`)**: Automatically runs `ivg-doctor.sh`, `renovate-sync.sh`, and `emacs-doctor.sh`.

### 2. Manual Inspection Points

If an anomaly is detected by the script, follow these steps to investigate:

#### A. Unused Requirement Candidates
* Search for any occurrences of the package name across the repository:
  ```bash
  git grep -i "<tool-name>"
  ```
* If the tool only appears in `requirements/*` and `renovate.*`, determine whether it is:
  1. An interactive CLI tool deliberately installed for manual terminal use (e.g., `evcxr_repl`, `bacon`).
  2. An obsolete tool or leftover from an earlier setup (e.g., `pylsp` plugins when using Ruff, or `debugpy` without DAP).
* If obsolete, remove it from `requirements/<category>` and review if any custom regex in `renovate.yml` needs adjustment.

#### B. Orphaned Config References
* When tools are removed from `requirements/*` or `.Brewfile`, check if editor configs still reference them:
  * Check formatter definitions: [my-reformatter.el](file:///.emacs.d/site-lisp/my-reformatter.el)
  * Check LSP / linter hooks: [.emacs.d/init.el](file:///.emacs.d/init.el)
* Example: If `rufo` was removed from `requirements/gem`, update `my-reformatter-ruby-format` to use `rubocop` or another available formatter.

#### C. Size & Style Violations
* If `.github/bin/lint.sh` fails due to line count thresholds (e.g., > 200 lines for bash, > 100 lines for zsh):
  * Inspect the offending script.
  * Modularize or extract helper functions into `bin/common.sh` or distinct scripts.

---

## Verification Checklist

Before committing changes after running the doctor:
- [ ] Run `.github/bin/lint.sh` and ensure all scripts are within line thresholds.
- [ ] Run `.github/bin/shellcheck.sh` and ensure zero warnings.
- [ ] Verify that `requirements/*` contains only actively needed or intentionally installed packages.
- [ ] If `renovate.yml` was modified, regenerate `renovate.json`:
  ```bash
  yq -o json renovate.yml > renovate.json
  ```
