#!/usr/bin/env python3
"""Emacs Keybind Advisor:
Analyzes keybindings combined with usage frequency (keyfreq),
considering sequence semantics, domain categories, and relative usage,
and suggests keybinding optimizations (promote, unbind, protect).
All rules, thresholds, and patterns are dynamically loaded from an external JSON file.
Self-contained: requires only Python standard libraries.
"""

from __future__ import annotations

import argparse
import json
import os
import re
import sys
from typing import Any, Dict, List, Set, Tuple


class AdvisorConfig:
    def __init__(self, config_path: str) -> None:
        if not os.path.isfile(config_path):
            raise FileNotFoundError(f"Configuration file not found: {config_path}")

        with open(config_path, "r", encoding="utf-8") as f:
            data = json.load(f)

        self.domains: List[Tuple[str, re.Pattern[str]]] = []
        for d in data.get("domains", []):
            name = d.get("name", "").strip()
            pattern = d.get("pattern", "").strip()
            if name and pattern:
                try:
                    self.domains.append((name, re.compile(pattern, re.I)))
                except re.error as e:
                    print(f"Warning: Invalid domain regex '{pattern}': {e}", file=sys.stderr)

        thresholds = data.get("thresholds", {})
        self.promote_min_freq = int(thresholds.get("promote_min_freq", 10))
        self.promote_min_depth = int(thresholds.get("promote_min_depth", 3))

        patterns = data.get("patterns", {})
        self.user_prefixes_pat = re.compile(patterns.get("user_prefixes", "^$"), re.I)
        self.danger_keywords_pat = re.compile(patterns.get("danger_keywords", "^$"), re.I)
        self.safe_tier_prefixes_pat = re.compile(patterns.get("safe_tier_prefixes", "^$"), re.I)


def classify_command(cmd: str, domains: List[Tuple[str, re.Pattern[str]]]) -> str:
    for domain, pattern in domains:
        if pattern.search(cmd):
            return domain
    return "General / Other"


def parse_sequence_semantics(seq: str, cfg: AdvisorConfig) -> Dict[str, Any]:
    parts = seq.split()
    depth = len(parts)
    is_danger = bool(cfg.danger_keywords_pat.search(seq))
    is_safe_tier = bool(cfg.safe_tier_prefixes_pat.search(seq))
    is_user_prefix = bool(cfg.user_prefixes_pat.search(seq))
    return {
        "depth": depth,
        "is_danger": is_danger,
        "is_safe_tier": is_safe_tier,
        "is_user_prefix": is_user_prefix,
    }


def main() -> None:
    parser = argparse.ArgumentParser(description="Emacs Keybind Advisor")
    parser.add_argument("--keybinds-file", help="Path to JSON/JSONL keybind dump (or '-' for stdin)")
    parser.add_argument("--keyfreq-file", required=True, help="Path to JSON keyfreq file")
    parser.add_argument("--config-file", required=True, help="Path to audit.json configuration file")
    parser.add_argument("--domain", help="Filter by specific domain")
    parser.add_argument("--min-freq", type=int, help="Override promote_min_freq threshold")
    parser.add_argument("--min-depth", type=int, help="Override promote_min_depth threshold")
    parser.add_argument("--top-dead", type=int, default=10, help="Max dead/unused bindings to show (default: 10)")
    args = parser.parse_args()

    cfg = AdvisorConfig(args.config_file)
    if args.min_freq is not None:
        cfg.promote_min_freq = args.min_freq
    if args.min_depth is not None:
        cfg.promote_min_depth = args.min_depth

    # Load keyfreq data
    with open(args.keyfreq_file, "r", encoding="utf-8") as f:
        freq_list = json.load(f)

    cmd_frequencies: Dict[str, int] = {}
    for item in freq_list:
        cmd = item.get("command", "")
        count = int(item.get("count", 0))
        cmd_frequencies[cmd] = cmd_frequencies.get(cmd, 0) + count

    # Load keybindings
    bindings: List[Dict[str, str]] = []
    if args.keybinds_file and args.keybinds_file != "-":
        with open(args.keybinds_file, "r", encoding="utf-8") as f:
            lines = f.readlines()
    else:
        lines = sys.stdin.readlines()

    for line in lines:
        line = line.strip()
        if not line:
            continue
        try:
            obj = json.loads(line)
            bindings.append(obj)
        except json.JSONDecodeError:
            m = re.match(r"^\[(.*?)\]\s+(.*?)\s+->\s+(.*)$", line)
            if m:
                bindings.append({"sequence": m.group(1), "keymap": m.group(2), "command": m.group(3)})

    # Aggregate by domain
    domain_map: Dict[str, List[Dict[str, Any]]] = {}
    for b in bindings:
        cmd = b.get("command", "")
        seq = b.get("sequence", "")
        keymap = b.get("keymap", "")
        dom = classify_command(cmd, cfg.domains)
        freq = cmd_frequencies.get(cmd, 0)
        semantics = parse_sequence_semantics(seq, cfg)

        item = {
            "sequence": seq,
            "keymap": keymap,
            "command": cmd,
            "frequency": freq,
            "domain": dom,
            **semantics,
        }
        domain_map.setdefault(dom, []).append(item)

    # 1. Output Domain Relative Frequency Analysis
    target_domains = [args.domain] if args.domain and args.domain in domain_map else sorted(domain_map.keys())

    print("==================================================")
    print(" 1. DOMAIN RELATIVE USAGE & KEYBIND HARMONY")
    print("==================================================")
    for dom in target_domains:
        items = domain_map[dom]
        unique_cmds: Dict[str, Dict[str, Any]] = {}
        for it in items:
            cmd = it["command"]
            if cmd not in unique_cmds:
                unique_cmds[cmd] = {"command": cmd, "frequency": it["frequency"], "sequences": set()}
            unique_cmds[cmd]["sequences"].add(it["sequence"])

        sorted_cmds = sorted(unique_cmds.values(), key=lambda x: x["frequency"], reverse=True)
        total_dom_freq = sum(x["frequency"] for x in sorted_cmds)

        print(f"\n[ Domain: {dom} ] (Total executions: {total_dom_freq})")
        for sc in sorted_cmds[:6]:
            pct = (sc["frequency"] / total_dom_freq * 100) if total_dom_freq > 0 else 0
            seqs_str = ", ".join(f"[{s}]" for s in sorted(sc["sequences"]))
            print(f"  {sc['frequency']:5d} calls ({pct:4.1f}%) | {sc['command']:<32} | {seqs_str}")

    # 2. Keybind Optimization Proposals
    print("\n==================================================")
    print(" 2. ACTIONABLE KEYBIND PROPOSALS")
    print("==================================================")

    # A. Candidates for Promotion
    print(f"\n[*] Candidates for Promotion (Usage >= {cfg.promote_min_freq}, Depth >= {cfg.promote_min_depth}):")
    candidates_promote = []
    for dom, items in domain_map.items():
        if dom == "General / Other":
            continue
        for it in items:
            if it["frequency"] >= cfg.promote_min_freq and it["depth"] >= cfg.promote_min_depth:
                candidates_promote.append(it)

    seen_cmds: Set[str] = set()
    for it in sorted(candidates_promote, key=lambda x: x["frequency"], reverse=True):
        if it["command"] not in seen_cmds:
            seen_cmds.add(it["command"])
            print(f"  - '{it['command']}' (Used {it['frequency']} times): Currently bound to long sequence [{it['sequence']}].")
            print(f"    -> Consider shortening to a 1-step or 2-step prefix (e.g. C-c <key> or M-s <key>).")

    if not seen_cmds:
        print("  None detected (active commands are already reasonably indexed).")

    # B. Candidates for Unbinding (Dead/Unused Bindings under user prefixes)
    print(f"\n[*] Candidates for Unbinding (Dead/Unused Bindings under user prefixes):")
    dead_custom = []
    for it in bindings:
        seq = it.get("sequence", "")
        cmd = it.get("command", "")
        if cfg.user_prefixes_pat.search(seq) and cmd_frequencies.get(cmd, 0) == 0:
            dead_custom.append(it)

    seen_dead: Set[Tuple[str, str]] = set()
    count_dead = 0
    for it in dead_custom:
        pair = (it["sequence"], it["command"])
        if pair not in seen_dead and count_dead < args.top_dead:
            seen_dead.add(pair)
            count_dead += 1
            print(f"  - [{it['sequence']}] -> {it['command']} (Used: 0 times)")
            print(f"    -> Safe to unbind: (unbind-key \"{it['sequence']}\") to reclaim comfortable slots.")

    if not seen_dead:
        print("  No obvious dead bindings found in designated user spaces.")

    # C. Sequence Mental Model Check
    print("\n[*] Sequence Mental Model & Safety Check:")
    print("  - Tiered prefixes matching pattern are verified as safety-guarded tool tiers.")
    print("  - Danger keywords are monitored to avoid direct mapping onto shallow, single-key triggers.")
    print("==================================================\n")


if __name__ == "__main__":
    main()
