#!/bin/bash
set -euo pipefail

# audit.sh:
# Investigation and auditing tool for Emacs keybindings and command usage.
# Combines:
#   - bin/emacs-keybind-dump.sh (keybinding definition & collision audit)
#   - bin/emacs-keyfreq-json.sh (real command execution frequency audit)
#   - scripts/advisor.py (smart keybinding proposals & semantics)

d=$(cd "$(dirname "$0")"/../../../.. || exit 1; pwd)
keybind_sh="${d}/bin/emacs-keybind-dump.sh"
keyfreq_sh="${d}/bin/emacs-keyfreq-json.sh"
advisor_py="${d}/.agents/skills/emacs-keybind-audit/scripts/advisor.py"
advisor_conf="${d}/.agents/skills/emacs-keybind-audit/resources/audit.json"

usage() {
    cat <<EOF
Usage: $(basename "$0") <COMMAND> [OPTIONS]

Keybinding Audit Commands:
  top [LIMIT]             Show top conflicting key sequences (default limit: 10)
  find-seq PATTERN        Find keymaps and commands for sequence matching regex PATTERN
  find-cmd PATTERN        Find key sequences bound to command matching regex PATTERN
  map-stats [LIMIT]       Show keymaps with the most keybindings (default limit: 10)
  dump [ARGS...]          Run raw keybind-dump via bin/emacs-keybind-dump.sh

Key Frequency (keyfreq) Audit Commands:
  freq-top [LIMIT]        Show most frequently used commands across all modes (default: 10)
  freq-mode [MODE]        Show command usage for a specific mode or mode breakdown
  freq-cmd PATTERN        Search execution frequencies of commands matching regex PATTERN
  freq-json               Output raw keyfreq statistics as JSON

Optimization & Proposal Commands:
  advise [DOMAIN]         Analyze semantics & relative usage, and propose keybinding optimizations

Examples:
  $(basename "$0") top 5
  $(basename "$0") find-seq "^M-s"
  $(basename "$0") find-cmd "deadgrep"
  $(basename "$0") freq-top 10
  $(basename "$0") freq-cmd "helm"
  $(basename "$0") advise
  $(basename "$0") advise "LSP & DevTools"
EOF
}

if [ $# -lt 1 ]; then
    usage
    exit 1
fi

cmd="$1"
shift

require_jq() {
    if ! command -v jq >/dev/null 2>&1; then
        echo "Error: jq is required for keyfreq analysis" >&2
        exit 1
    fi
}

case "${cmd}" in
    top)
        limit="${1:-10}"
        echo "=== Top ${limit} Conflicting Sequences ==="
        "${keybind_sh}" -d "${HOME}/.emacs.d" --stats sequence --limit "${limit}"
        ;;
    find-seq)
        if [ $# -lt 1 ]; then
            echo "Error: PATTERN required for find-seq" >&2
            exit 1
        fi
        pat="$1"
        shift
        echo "=== Keybindings for Sequence Pattern: '${pat}' ==="
        "${keybind_sh}" -d "${HOME}/.emacs.d" -s "${pat}" "$@"
        ;;
    find-cmd)
        if [ $# -lt 1 ]; then
            echo "Error: PATTERN required for find-cmd" >&2
            exit 1
        fi
        pat="$1"
        shift
        echo "=== Keybindings for Command Pattern: '${pat}' ==="
        "${keybind_sh}" -d "${HOME}/.emacs.d" -c "${pat}" "$@"
        ;;
    map-stats)
        limit="${1:-10}"
        echo "=== Keymap Statistics (Top ${limit}) ==="
        "${keybind_sh}" -d "${HOME}/.emacs.d" --stats keymap --limit "${limit}"
        ;;
    dump)
        "${keybind_sh}" -d "${HOME}/.emacs.d" "$@"
        ;;
    freq-top)
        require_jq
        limit="${1:-10}"
        echo "=== Top ${limit} Most Frequently Executed Commands ==="
        "${keyfreq_sh}" | jq -r --argjson n "${limit}" '
          group_by(.command)
          | map({command: .[0].command, total: (map(.count) | add)})
          | sort_by(-.total)
          | .[:$n][]
          | "\(.total)\t\(.command)"'
        ;;
    freq-mode)
        require_jq
        mode="${1:-}"
        limit="${2:-10}"
        if [ -n "${mode}" ]; then
            echo "=== Top Commands in Mode: '${mode}' ==="
            "${keyfreq_sh}" | jq -r --arg m "${mode}" --argjson n "${limit}" '
              map(select(.mode == $m))
              | sort_by(-.count)
              | .[:$n][]
              | "\(.count)\t\(.command)"'
        else
            echo "=== Top Modes by Total Command Executions ==="
            "${keyfreq_sh}" | jq -r --argjson n "${limit}" '
              group_by(.mode)
              | map({mode: .[0].mode, total: (map(.count) | add)})
              | sort_by(-.total)
              | .[:$n][]
              | "\(.total)\t\(.mode)"'
        fi
        ;;
    freq-cmd)
        require_jq
        if [ $# -lt 1 ]; then
            echo "Error: PATTERN required for freq-cmd" >&2
            exit 1
        fi
        pat="$1"
        echo "=== Execution Frequency for Command Pattern: '${pat}' ==="
        "${keyfreq_sh}" | jq -r --arg p "${pat}" '
          map(select(.command | test($p)))
          | sort_by(-.count)[]
          | "\(.count)\t[\(.mode)] \(.command)"'
        ;;
    freq-json)
        "${keyfreq_sh}" "$@"
        ;;
    advise)
        domain="${1:-}"
        tmp_freq="$(mktemp)"
        trap 'rm -f "$tmp_freq"' EXIT
        "${keyfreq_sh}" > "$tmp_freq"

        extra_args=(--config-file "${advisor_conf}")
        if [ -n "${domain}" ]; then
            extra_args+=(--domain "${domain}")
        fi

        "${keybind_sh}" -d "${HOME}/.emacs.d" -f jsonl | python3 "${advisor_py}" --keyfreq-file "${tmp_freq}" "${extra_args[@]}"
        ;;
    -h|--help|help)
        usage
        exit 0
        ;;
    *)
        echo "Unknown command: ${cmd}" >&2
        usage >&2
        exit 1
        ;;
esac
