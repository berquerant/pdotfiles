---
name: emacs-healthcheck
description: >-
  Inspect Emacs configuration integrity, detect keybinding conflicts, verify external
  toolchains (LSP, formatters, treesit), and troubleshoot startup issues in batch mode.
  Use this skill when modifying .emacs.d/, diagnosing package loading errors, or checking
  keymap overlaps.
---

# Emacs Healthcheck

This skill provides diagnostic procedures and maintenance workflows for the modular Emacs environment (supporting both GUI and CUI configurations, straight.el package management, native compilation, and external formatter integrations).

## Architecture

* **Configuration Root**: [.emacs.d](file:///.emacs.d)
  * [early-init.el](file:///.emacs.d/early-init.el): Early startup optimizations and frame styling.
  * [init.el](file:///.emacs.d/init.el): Main packages, key bindings, modes, and hooks.
  * [site-lisp/](file:///.emacs.d/site-lisp): Modular custom extensions (`my-reformatter.el`, `my-flycheck-golangci-lint.el`, `my-git-browse.el`, etc.).
  * [straight-default.el](file:///.emacs.d/straight-default.el): Pinned package lock commit hashes.
  * [treesit-language-source-alist.el](file:///.emacs.d/treesit-language-source-alist.el): Tree-sitter grammar upstream repositories.
* **Execution Binaries & Directories**:
  * GUI (`EMACSD=~/.emacs.d`): `/Applications/Emacs-GUI.app` (aliased as `emacs`, `gmacs`)
  * CUI (`CMACSD=~/.cmacs.d`): `/usr/local/bin/emacs --init-directory="$CMACSD"` (aliased as `cmacs`)
  * Minimal: `bin/emacs-light.sh` (`lmacs`), `bin/emacs-less.sh` (`umacs`)
  * **Important**: `emacs` in `PATH` (e.g. `/opt/homebrew/bin/emacs`) may differ in Emacs version or build options (such as built-in `sqlite3`) from `/usr/local/bin/emacs` (`cmacs`). Always match `EMACSD` and the target binary when diagnosing CUI vs GUI issues.
* **Maintenance Scripts**:
  * `bin/emacs-batch.sh`: Headless batch invocation.
  * `bin/emacs-keybind-dump.sh`: Dumps mode keybindings (used by `emacs-keybind-audit`).
  * `bin/clean-emacs.sh`: Prunes native-comp / straight caches.

---

## Diagnostic Workflows

### 1. Run Comprehensive Emacs Health Check

Execute the bundled diagnostic script from the repository root:

```bash
bash .agents/skills/emacs-healthcheck/scripts/emacs-doctor.sh
```

The script verifies:
1. **Batch Startup**: Evaluates `.emacs.d/init.el` in headless batch mode for syntax or macro-expansion errors.
2. **Keybinding Audit Capability**: Confirms readiness of `emacs-keybind-audit`.
3. **Reformatter Executables**: Checks that formatters declared in `my-reformatter.el` exist in PATH.
4. **External Dependencies & Paths**: Confirms availability of `migemo` dictionary, `libvterm`, and `tree-sitter`.

### 2. Investigating Keybinding Conflicts & Usage

For deep inspection, collision detection, and command usage frequency analysis, switch to the dedicated skill:
* **Dedicated Skill**: `emacs-keybind-audit` ([.agents/skills/emacs-keybind-audit](.agents/skills/emacs-keybind-audit))

```bash
# Show top conflicting key sequences
bash .agents/skills/emacs-keybind-audit/scripts/audit.sh top 10

# Search key combinations bound to a specific command
bash .agents/skills/emacs-keybind-audit/scripts/audit.sh find-cmd "consult"

# Get optimization and promotion advice
bash .agents/skills/emacs-keybind-audit/scripts/audit.sh advise
```

Common resolution strategies:
* Prefix mode-specific bindings under distinct prefix keys (e.g., `C-c ...` or `M-s M-s ...`).
* Unbind conflicting global keys before binding mode maps using `(unbind-key "..." ...)`.

### 3. Cleaning Caches & Troubleshooting Straight Packages

When packages fail to compile, native compilation behaves erratically, or stale dependencies (e.g., `compat`, `transient`) cause macro/version mismatch errors (`void-function: static-when`, `cl-generic-ensure-function`, `Magit requires 'transient' >= ...`):

```bash
# Target CUI (~/.cmacs.d) by setting EMACSD="$CMACSD" (omit for GUI ~/.emacs.d)
# Delete byte-compiled files (.elc), native comp cache (.eln), and ELPA cache
EMACSD="$CMACSD" bin/clean-emacs.sh cache

# Delete and reset stale or corrupted straight.el packages using stride
EMACSD="$CMACSD" bin/clean-emacs.sh straight <package_name>
```

After cleaning/updating stale dependencies (such as `compat` and `transient`), rebuild and verify **actual command execution** in the target environment rather than only checking `init.el` load:

```bash
EMACSD="$CMACSD" /usr/local/bin/emacs --init-directory="$CMACSD" \
  --batch --quick --load "$CMACSD/early-init.el" --load "$CMACSD/init.el" \
  --eval '(progn (find-file "README.md") (magit-status) (magit-dispatch))'
```

### 4. Updating Package Locks & Treesit

* **Straight Lock**: Tracked in `.emacs.d/straight-default.el`. Run `(straight-freeze-versions)` in batch mode against the target directory (e.g. `EMACSD="$CMACSD"`) after adding or upgrading packages.
* **Tree-sitter Grammars**: Version pinned in `.emacs.d/treesit-language-source-alist.el` (managed and updated via Renovate).

---

## Verification Checklist

After editing Emacs Lisp files:
- [ ] Run `bash .agents/skills/emacs-healthcheck/scripts/emacs-doctor.sh` to ensure clean batch initialization.
- [ ] For CUI changes, verify against `EMACSD="$CMACSD"` and `/usr/local/bin/emacs --init-directory="$CMACSD"`, including executing the newly added commands (not just loading `init.el`).
- [ ] If changing keybindings, verify conflicts via `bash .agents/skills/emacs-keybind-audit/scripts/audit.sh top 10`.
- [ ] Verify formatting with `my-reformatter-format` if modifying reformatter definitions.
