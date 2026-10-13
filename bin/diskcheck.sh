#!/bin/bash

d="$(
  cd "$(dirname "$0")"/.. || exit 1
  pwd
)"
. "${d}/bin/common.sh"

# find files or directories >= 5GB
# $1: path (default: /)
# $2: depth (default: 5)
diskcheck() {
  cecho green "=== Disk usage (over 5GB, sorted largest first) ==="
  sudo du -m -x -d "${2:-5}" "${1:-/}" 2>/dev/null |\
    awk '$1 >= 5120 {print $1, length($2), $2}' |\
    sort -nr |\
    awk '{
          gb = $1 / 1024;
          printf "%.2fGB\t%s\n", gb, $3
    }'
}

fdcheck() {
  local __tmp
  __tmp=$(mktemp)
  trap 'rm -f "$__tmp"' EXIT

  if ! sudo lsof -n -P 2>/dev/null > "$__tmp"; then
    cecho red "failed to execute lsof" >&2
    exit 1
  fi

  cecho green "=== Sum ==="
  tail -n +2 "$__tmp" | wc -l

  echo
  cecho green "=== Worst 10 processes (FD, PID, NAME) ==="
  awk 'NR>1 {pid[$2]++; name[$2]=$1} END {for (p in pid) print pid[p], p, name[p]}' "$__tmp" | sort -nr | head -n 10
}

usage() {
  cat <<EOS
${0##*/} [PATH] [DEPTH]
${0##*/} disk [PATH] [DEPTH]
  check disk usage
${0##*/} fd
  check fd usage
EOS
}

main() {
  case "$1" in
    "-h" | "--help") usage ;;
    "") diskcheck ;;
    "disk") shift ; diskcheck ;;
    "fd") fdcheck ;;
    *) usage ; exit 1 ;;
  esac
}

main "$@"
