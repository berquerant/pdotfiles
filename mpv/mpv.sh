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

ffraw() {
    if [ ! -f "$MF_INDEX" ] ; then
        ffreload
    fi
    mf -i "$MF_INDEX_NORMALIZED" "$@"
}

ffquery() {
    ffraw "$@"
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
