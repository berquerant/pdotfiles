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
* **Execution Binaries**:
  * GUI: `/Applications/Emacs-GUI.app` (aliased as `emacs`, `gmacs`)
  * CUI: `/usr/local/bin/emacs` (aliased as `cmacs`)
  * Minimal: `bin/emacs-light.sh` (`lmacs`), `bin/emacs-less.sh` (`umacs`)
* **Maintenance Scripts**:
  * `bin/emacs-batch.sh`: Headless batch invocation.
  * `bin/emacs-key-conflict.sh`: Scans for duplicate keybindings.
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
2. **Keybinding Conflict Detection**: Runs `bin/emacs-key-conflict.sh r` to check for overlapping hotkeys.
3. **Reformatter Executables**: Checks that formatters declared in `my-reformatter.el` exist in PATH.
4. **External Dependencies & Paths**: Confirms availability of `migemo` dictionary, `libvterm`, and `tree-sitter`.

### 2. Investigating Keybinding Conflicts

To locate and resolve key binding collisions:

```bash
# Print raw conflicting key sequences
bin/emacs-key-conflict.sh r

# Search where a specific key sequence is defined
bin/emacs-key-conflict.sh seq "C-c p"
```

Common resolution strategies:
* Prefix mode-specific bindings under distinct prefix keys (e.g., `C-c ...` or `M-s M-s ...`).
* Unbind conflicting global keys before binding mode maps using `(unbind-key "..." ...)`.

### 3. Cleaning Caches & Straight Packages

When packages fail to compile, native compilation behaves erratically, or cache drift occurs:

```bash
# Delete byte-compiled files (.elc), native comp cache (.eln), and ELPA cache
bin/clean-emacs.sh cache

# Prune unneeded or corrupted straight.el repositories using stride
bin/clean-emacs.sh straight [optional_package_name]
```

### 4. Updating Package Locks & Treesit

* **Straight Lock**: Tracked in `.emacs.d/straight-default.el`.
* **Tree-sitter Grammars**: Version pinned in `.emacs.d/treesit-language-source-alist.el` (managed and updated via Renovate).

---

## Verification Checklist

After editing Emacs Lisp files:
- [ ] Run `bash .agents/skills/emacs-healthcheck/scripts/emacs-doctor.sh` to ensure clean batch initialization.
- [ ] Verify no unexpected key conflict was introduced via `bin/emacs-key-conflict.sh r`.
- [ ] Verify formatting with `my-reformatter-format` if modifying reformatter definitions.
