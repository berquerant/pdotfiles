---
name: emacs-keybind-audit
description: >-
  Inspect, audit, and analyze Emacs keybindings across active keymaps and modes using
  bin/emacs-keybind-dump.sh, and investigate actual command execution usage frequencies
  via bin/emacs-keyfreq-json.sh. Use this skill when investigating keybinding collisions,
  identifying key sequences shared across multiple modes, finding what command is bound to a
  shortcut, or analyzing real user command invocation frequency.
---

# Emacs Keybind & Usage Audit

This skill provides procedures for dumping, searching, and analyzing Emacs keybindings across configured modes using `bin/emacs-keybind-dump.sh` (powered by `emacs-keybind-dump`), as well as auditing real command execution frequencies recorded by `keyfreq` using `bin/emacs-keyfreq-json.sh`.

## Architecture & Integration

* **Keybind Dumper Tool**: `emacs-keybind-dump` (installed via pip from git repo).
* **Mode Preloader**: `bin/emacs-extract-modes.sh` scans `.emacs.d` and generates batch loader forms so that minor/major modes are active when dumping.
* **Wrapper Script**: `bin/emacs-keybind-dump.sh` preloads mode definitions and chains `keybind-dump` pipeline filtering.
* **Usage Frequency Tool**: `bin/emacs-keyfreq-json.sh` parses `~/.emacs.d/.keyfreq` S-expressions into structured JSON objects.
* **Audit Helper**: `.agents/skills/emacs-keybind-audit/scripts/audit.sh` provides unified subcommands for both static keybinding analysis and dynamic execution frequency auditing.

---

## Diagnostic & Audit Workflows

### 1. Keybinding Definition & Conflict Audits

```bash
# Show top 5 or 10 conflicting key sequences
bash .agents/skills/emacs-keybind-audit/scripts/audit.sh top 10

# Search keybindings by sequence regex pattern
bash .agents/skills/emacs-keybind-audit/scripts/audit.sh find-seq "^M-s"
bash .agents/skills/emacs-keybind-audit/scripts/audit.sh find-seq "^C-x"

# Search key combinations bound to a specific command
bash .agents/skills/emacs-keybind-audit/scripts/audit.sh find-cmd "deadgrep"
bash .agents/skills/emacs-keybind-audit/scripts/audit.sh find-cmd "consult"

# Show keymaps with the most keybindings
bash .agents/skills/emacs-keybind-audit/scripts/audit.sh map-stats 10
```

### 2. Command Execution Frequency Audits (keyfreq)

Analyze what commands the user actually runs in practice to evaluate whether shortcuts are worth preserving or remapping:

```bash
# Show most frequently executed commands across all modes
bash .agents/skills/emacs-keybind-audit/scripts/audit.sh freq-top 10

# Show command usage breakdown for a specific mode
bash .agents/skills/emacs-keybind-audit/scripts/audit.sh freq-mode go-mode 10
bash .agents/skills/emacs-keybind-audit/scripts/audit.sh freq-mode emacs-lisp-mode 10

# Show modes ordered by total command executions
bash .agents/skills/emacs-keybind-audit/scripts/audit.sh freq-mode

# Search execution frequencies for commands matching a regex pattern
bash .agents/skills/emacs-keybind-audit/scripts/audit.sh freq-cmd "avy"
bash .agents/skills/emacs-keybind-audit/scripts/audit.sh freq-cmd "git"

# Output raw keyfreq statistics as JSON for custom jq / Python processing
bash .agents/skills/emacs-keybind-audit/scripts/audit.sh freq-json | jq '.[0:5]'
```

---

## Verification & Conflict Resolution Guidelines

When analyzing keybinding conflicts:
1. **Correlate with keyfreq data**:
   - Before removing or reassigning a shortcut, check `audit.sh freq-cmd "<target-cmd>"` to see if it is actively utilized.
2. **Differentiate intentional context overlays vs. collisions**:
   - `same-cmd`: Same command is bound in multiple modes (harmless consistency).
   - Global vs. dedicated mode: Overriding a global key inside a dedicated viewer (e.g., `Buffer-menu-mode-map` overriding `C-o`) is standard Emacs design.
   - Minor mode conflicts: Check if two concurrently active minor modes (e.g., `dumb-jump-mode` vs. language modes) override the same prefix without mutual exclusivity.
3. **Mitigation**:
   - Move personal bindings to the user prefix `C-c <letter>` or `M-s ...`.
   - Use `(unbind-key "..." <keymap>)` to suppress intrusive third-party bindings.

---

## Configuration & Customization (`audit.json`)

All domain classifications, frequency/depth thresholds, and pattern matching rules are defined in an external JSON configuration file:
* **Config file**: [`resources/audit.json`](resources/audit.json)

### Structure & How to Update
```json
{
  "description": "Configuration for Emacs Keybind Audit & Advisor",
  "domains": [
    {
      "name": "AI & Chat",
      "pattern": "^(ellama|gptel|chatgpt|openai)",
      "description": "Generative AI and chat buffer operations"
    },
    {
      "name": "LSP & DevTools",
      "pattern": "^(lsp|eglot|flycheck|flymake|dap-|godef|xref)",
      "description": "Language Server Protocols and IDE tools"
    }
  ],
  "thresholds": {
    "promote_min_freq": 10,
    "promote_min_depth": 3,
    "description": "Criteria to consider a command frequently used and candidates for shortening"
  },
  "patterns": {
    "user_prefixes": "^(M-s|C-c|C-x a)",
    "danger_keywords": "\\b(kill|delete|abort|quit|shutdown)\\b",
    "safe_tier_prefixes": "^(M-s\\s+M-s|C-c\\s+C-c)",
    "description": "Prefix and safety patterns for sequence classification"
  }
}
```

1. **Domains**: Order matters (first matched domain wins). Add or adjust patterns as new packages are integrated.
2. **Thresholds**: Adjust `promote_min_freq` (execution count) and `promote_min_depth` (number of strokes) to customize promotion candidates.
3. **Patterns**: Define `user_prefixes` (custom spaces checked for dead/unused keys) and `danger_keywords` (monitored to ensure safety tiers).
4. Run `bash .agents/skills/emacs-keybind-audit/scripts/audit.sh advise` to apply changes immediately.

---

## Domain Updating Procedure (from `.emacs.d` `use-package`)

To maximize classification accuracy, domains should be fine-grained and aligned with the actual packages loaded in `.emacs.d`:

### Step 1: Extract all `use-package` declarations
Run a quick search across `.emacs.d` to list all managed packages:
```bash
python3 -c "
import glob, re
pkgs = set()
for f in glob.glob('.emacs.d/**/*.el', recursive=True):
    with open(f, encoding='utf-8', errors='ignore') as fp:
        for m in re.finditer(r'\(use-package\s+([a-zA-Z0-9_\-]+)', fp.read()):
            pkgs.add(m.group(1))
for p in sorted(pkgs):
    print(p)
"
```

Verify: The command outputs a sorted non-empty list of unique package names extracted from `.emacs.d/`.

### Step 2: Categorize packages into functional domains
For each package, determine its primary role (using Emacs docstrings `C-h P <pkg>`, package comments, or web search if unfamiliar) and group them:
* **LSP & Code Intelligence**: `lsp-mode`, `eglot`, `consult-eglot`
* **Syntax & Diagnostics**: `flycheck`, `flymake-ruff`, `flymake-diagnostic-at-point`
* **Completion & Minibuffer**: `company`, `vertico`, `orderless`, `marginalia`, `embark`, `prescient`
* **Search & Grep**: `isearch`, `consult`, `deadgrep`, `grep`, `occur`, `affe`
* **Navigation & Jump**: `avy-migemo`, `dumb-jump`, `goto-chg`, `bm`, `scroll-util`
* **Selection & Structural Editing**: `multiple-cursors`, `expand-region`, `smartparens`, `selected`, `mwim`
* **Editing, Formatting & Replace**: `reformatter`, `comment-dwim-2`, `ez-query-replace`, `anzu`, `whitespace`
* **Buffer, Window & Frame**: `popwin`, `buffer-change`, `my-sticky-buffer-mode`
* **Project & Workspace**: `projectile`, `consult-ghq`, `treemacs`
* **Files & Dired**: `dired`, `dired-x`, `my-open-file`, `my-path`, `goto-addr`, `browse-url`
* **Version Control & Git**: `diff-hl`, `ediff`, `dsvn`, `git-complete`, `my-git-browse`
* **Language Modes & Playgrounds**: `go-mode`, `rust-mode`, `python`, `ruby-mode`, `go-playground`, `rust-playground`, etc.
* **Terminal & Shell**: `vterm`, `tramp`, `exec-path-from-shell`
* **Restclient & Chat/AI**: `restclient`, `thread-buffer`, `thread-buffer-chat`, `message-routing`
* **Themes & Visual UI**: `modus-themes`, `solarized`, `rainbow-mode`, `which-key`, `posframe`, `diminish`

Verify: Every extracted package is mapped to an intended functional domain or deliberately assigned to general defaults.

### Step 3: Update `resources/audit.json`
Construct regex prefixes for each domain based on the package function names and command naming conventions (e.g. `^(lsp|eglot|consult-eglot|xref)`). Place more specific domains higher than general ones.

Verify: `jq . .agents/skills/emacs-keybind-audit/resources/audit.json > /dev/null` exits 0 with valid JSON syntax.

### Step 4: Verify Classification Coverage
Run `audit.sh advise` and ensure that meaningful commands are classified into specific domains rather than spilling into `General / Other`.

Verify: `bash .agents/skills/emacs-keybind-audit/scripts/audit.sh advise` runs without errors, lists the updated domains under "Domain Breakdown", and shows low command counts under "General / Other".
