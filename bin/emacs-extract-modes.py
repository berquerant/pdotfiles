#!/usr/bin/env python3
"""Extract modes configured via use-package and bind-key in Emacs configuration,
and generate loading scripts or commands.
"""

from __future__ import annotations

import argparse
import os
import re
import sys
from typing import Dict, List, Set


def find_config_el_files(emacs_dir: str) -> List[str]:
    """Find user configuration elisp files.

    If target directory has symlinked init.el or site-lisp, resolve them or
    only inspect the user config files (excluding package management data like straight, elpa).
    """
    target_files: List[str] = []

    # If emacs_dir points to a dotfiles repo directory (or ~/.emacs.d)
    real_dir = os.path.realpath(emacs_dir)

    for root, dirs, files in os.walk(emacs_dir, followlinks=False):
        # Exclude directories that contain packages or third-party code
        dirs[:] = [
            d
            for d in dirs
            if not d.startswith("straight")
            and not d.startswith("elpa")
            and not d.startswith(".")
            and d not in {"var", "eln-cache", "tree-sitter", "junk"}
        ]
        for file in files:
            if file.endswith(".el") and not file.startswith("."):
                target_files.append(os.path.join(root, file))

    # Also check if site-lisp is a symlink directly under emacs_dir
    site_lisp_link = os.path.join(emacs_dir, "site-lisp")
    if os.path.islink(site_lisp_link):
        resolved_site_lisp = os.path.realpath(site_lisp_link)
        for root, dirs, files in os.walk(resolved_site_lisp):
            for file in files:
                if file.endswith(".el") and not file.startswith("."):
                    full_p = os.path.join(root, file)
                    if full_p not in target_files:
                        target_files.append(full_p)

    return sorted(list(set(target_files)))


def parse_emacs_config(emacs_dir: str) -> Dict[str, Set[str]]:
    """Scan user elisp files in emacs_dir and extract configured modes."""
    use_package_modes: Set[str] = set()
    bind_key_modes: Set[str] = set()

    el_files = find_config_el_files(emacs_dir)

    for path in el_files:
        try:
            with open(path, "r", encoding="utf-8", errors="ignore") as f:
                text = f.read()
        except OSError as e:
            print(f"Warning: could not read {path}: {e}", file=sys.stderr)
            continue

        # 1. Package name in (use-package <pkg> ...)
        for m in re.finditer(r"\(use-package\s+([a-zA-Z0-9_\-]+)", text):
            pkg = m.group(1)
            if pkg.endswith("-mode"):
                use_package_modes.add(pkg)

        # 2. Modes in :mode ("\\.ext\\'" . <mode>) or :mode <mode>
        for m in re.finditer(r":mode\s+([^)\n]+)", text):
            val = m.group(1)
            for mode in re.findall(r"([a-zA-Z0-9_\-]+-mode)", val):
                use_package_modes.add(mode)

        # 3. Keymaps in bind-key, bind-keys, unbind-key, :bind (:map <map>)
        for m in re.finditer(r"([a-zA-Z0-9_\-]+-mode)-map", text):
            bind_key_modes.add(m.group(1))

    return {
        "use_package": use_package_modes,
        "bind_key": bind_key_modes,
    }


def generate_elisp_loader(modes: List[str]) -> str:
    """Generate Elisp code to load all given modes."""
    modes_str = " ".join(modes)
    return (
        f"(dolist (m '({modes_str}))\n"
        "  (condition-case err\n"
        "      (funcall m)\n"
        "    (error (message \"Failed to load mode %s: %s\" m err))))"
    )


def generate_shell_command(modes: List[str], init_dir: str) -> str:
    """Generate a shell command invoking Emacs or keybind-dump with the modes."""
    elisp = generate_elisp_loader(modes)
    return (
        f'emacs --batch -d "{init_dir}" -l "{init_dir}/init.el" --eval \\\n'
        f"  '{elisp}'"
    )


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Extract Emacs modes and generate loading scripts."
    )
    parser.add_argument(
        "--emacs-dir",
        default=os.path.expanduser("~/.emacs.d"),
        help="Path to Emacs config directory (default: ~/.emacs.d)",
    )
    parser.add_argument(
        "--format",
        choices=["list", "elisp", "command", "all"],
        default="all",
        help="Output format: list, elisp, command, or all (default: all)",
    )
    parser.add_argument(
        "--target",
        choices=["all", "use-package", "bind-key"],
        default="all",
        help="Target category of modes to include (default: all)",
    )

    args = parser.parse_args()
    data = parse_emacs_config(args.emacs_dir)

    up_modes = sorted(data["use_package"])
    bk_modes = sorted(data["bind_key"])
    all_modes = sorted(data["use_package"] | data["bind_key"])

    if args.target == "use-package":
        selected_modes = up_modes
    elif args.target == "bind-key":
        selected_modes = bk_modes
    else:
        selected_modes = all_modes

    if args.format == "list":
        for mode in selected_modes:
            print(mode)
    elif args.format == "elisp":
        print(generate_elisp_loader(selected_modes))
    elif args.format == "command":
        print(generate_shell_command(selected_modes, args.emacs_dir))
    else:
        print("=== Modes configured via use-package ===")
        for m in up_modes:
            print(f"  {m}")
        print(f"\nTotal: {len(up_modes)}\n")

        print("=== Modes with keybindings configured via bind-key / :bind ===")
        for m in bk_modes:
            print(f"  {m}")
        print(f"\nTotal: {len(bk_modes)}\n")

        print("=== All Unique Modes ===")
        for m in all_modes:
            print(f"  {m}")
        print(f"\nTotal: {len(all_modes)}\n")

        print("=== Generated Elisp Loader ===")
        print(generate_elisp_loader(all_modes))
        print("\n=== Generated Emacs Batch Command ===")
        print(generate_shell_command(all_modes, args.emacs_dir))


if __name__ == "__main__":
    main()
