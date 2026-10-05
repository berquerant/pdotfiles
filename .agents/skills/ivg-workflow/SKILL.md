---
name: ivg-workflow
description: >-
  Manage tools, builds, and dependencies orchestrated by IVG (Install Via Git).
  Use this skill when adding new IVG targets, updating locks, troubleshooting build failures,
  or running bulk installations of git-managed dependencies.
---

# IVG (Install Via Git) Workflow

This skill guides you through managing and troubleshooting applications installed and maintained via `ivg` (Install Via Git).

## Architecture Overview

IVG manages tools built from source or git repositories:
* **Target Definitions**: `ivg/<target>.yml` defining repository, branch, local clone directory, build/install commands, and uninstall commands.
* **Target Lists**:
  * [targets/util](file:///targets/util): Core utility tools (e.g., `docker-debian-emacs`, `gomodbrowse`).
  * [targets/other](file:///targets/other): Secondary applications (e.g., `meta-agent`).
  * [targets/additional](file:///targets/additional): Additional tools (e.g., `ndql`, `mpv-settings`).
* **Locks**:
  * `ivg/locks/<target>.lock`: Pinned git commit hashes.
  * `ivg/renovate.lock`: Aggregated lock manifest inspected and maintained by Renovate.
* **Scripts**:
  * `bin/install-via-git.sh`: Single target installer/updater.
  * `bin/install-via-git-bulk.sh`: Bulk installer for targets lists.
  * `bin/renovate-ivg.sh`: Generates or applies `renovate.lock` and target lockfiles.

---

## Common Workflows

### 1. Check IVG Integrity

Run the diagnostic script to verify configs, locks, and target lists:

```bash
bash .agents/skills/ivg-workflow/scripts/ivg-doctor.sh
```

### 2. Install or Update a Single Target

```bash
# Clean install using current lock
bin/install-via-git.sh <target>

# Update to latest git branch and update lock
bin/install-via-git.sh <target> --update

# Retry installation (skips git clone/pull if repo already exists)
bin/install-via-git.sh <target> --retry

# Uninstall target
bin/install-via-git-uninstall.sh <target>
```

### 3. Bulk Operations

Operate on target categories (`util`, `other`, `additional`):

```bash
# Bulk install
bin/install-via-git-bulk.sh < targets/<category>

# Bulk update
bin/install-via-git-bulk.sh --update < targets/<category>

# Bulk retry on failure
bin/install-via-git-bulk.sh --retry < targets/<category>
```

### 4. Lock Synchronization & Renovate Flow

When repositories receive new releases or when synchronizing with Renovate:

1. **Generate `renovate.lock`**:
   ```bash
   bin/renovate-ivg.sh gen
   ```
2. **Apply `renovate.lock` to individual target locks**:
   ```bash
   bin/renovate-ivg.sh lock
   ```
3. Verify that `git status` reflects changes in `ivg/locks/*.lock` and `ivg/renovate.lock`.

### 5. Adding a New Target

1. Create `ivg/<target>.yml`:
   ```yaml
   uri: https://github.com/<owner>/<repo>
   branch: main
   locald: repos/<target>
   lock: locks/<target>.lock
   install:
     - <build-command>
     - cp -f ./bin/<target> /usr/local/bin/<target>
   uninstall:
     - rm -f /usr/local/bin/<target>
   ```
2. Append `<target>` to the appropriate target list:
   * [targets/util](file:///targets/util)
   * [targets/other](file:///targets/other)
   * [targets/additional](file:///targets/additional)
3. Initial install and lock generation:
   ```bash
   bin/install-via-git.sh <target>
   bin/renovate-ivg.sh gen
   ```
4. Verify deployment and commit the definition and lockfile.

---

## Troubleshooting Guide

* **Build Failure in Local Repo**:
  * Inspect `ivg/repos/<target>` for intermediate artifacts or failed compilation.
  * Try retrying with `bin/install-via-git.sh <target> --retry`.
  * If a fresh build is needed, remove `ivg/repos/<target>` and rerun.
* **Missing Lockfile**:
  * If `ivg/locks/<target>.lock` is missing or out of sync, run `bin/install-via-git.sh <target> --update` or `bin/renovate-ivg.sh lock`.
* **Broken Symlink in `/usr/local/bin`**:
  * Re-run the install script for that target, or inspect `install:` steps in `ivg/<target>.yml`.
