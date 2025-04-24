#!/bin/bash

readonly d="$(cd "$(dirname "$0")" || exit; pwd)"

export MF_CONFIG="${d}/.mf.yml"
export MF_INDEX="${TMPD}/.mf.index.json"
export MF_INDEX_NORMALIZED="${TMPD}/.mf.index.normalized.json"

ffindex() {
    mf --config "$MF_CONFIG" -v > "$MF_INDEX"
}

ffnormalize() {
    # shellcheck disable=SC2016
    linep go -q -i 'golang.org/x/text/unicode/norm|encoding/json' 'type P struct {
  Path string `json:"path"`
}' 'var p P
if err := json.Unmarshal([]byte(x), &p); err != nil {
  fmt.Fprintf(os.Stderr, "%v\n", err)
  continue
}
n := norm.NFKC.String(x)
fmt.Printf("{\"path\":\"%s\",\"n\":%s}\n", p.Path, n)' < "$MF_INDEX" > "$MF_INDEX_NORMALIZED"
}

ffreload() {
    ffindex
    ffnormalize
}

ffquery() {
    if [ ! -f "$MF_INDEX" ] ; then
        ffreload
    fi
    mf -i "$MF_INDEX_NORMALIZED" "$@"
}

# name matches "xxx"
# name matches "yyy" and attr == "val"
# ->
# (name matches "xxx") or (name matches "yyy" and attr == "val")
fflist() {
    local -r tmp="$(mktemp)"
    linep py 'acc=[]' 'acc.append(f"({x})")' 'print(" or ".join(acc))' > "$tmp"
    ffquery -e "@${tmp}" "$@"
}

# ffshuffle [-n HEAD_COUNT] [--dry] [GREP_ARGS...] [-- FFLIST_ARGS...] < PLAYLIST
ffshuffle() {
    local grep_args=()
    local head_count=1000
    local dry=""
    local toggle_arg=""
    local list_args=()

    __ffshuffle() {
        fflist "${list_args[@]}" | rg "${grep_args[@]}" | shuf -n "$head_count"
    }

    while [[ $# -gt 0 ]] ; do
        case "$1" in
            "--dry")
                dry="true"
                shift
                ;;
            "-n")
                if [[ -z "$2" ]] ; then
                    echo >&2 '-n requires an argment'
                    return 1
                fi
                head_count="$2"
                shift 2
                ;;
            "--")
                toggle_arg="true"
                shift
                ;;
            *)
                if [[ "${toggle_arg}" = "true" ]] ; then
                    list_args+=("$1")
                else
                    grep_args+=("$1")
                fi
                shift
                ;;
        esac
    done

    if [[ -z "${grep_args[*]}" ]] ; then
        grep_args=(".*")
    fi

    if [[ "${dry}" = "true" ]] ; then
        __ffshuffle
    else
        __ffshuffle | mpv_play
    fi
}
