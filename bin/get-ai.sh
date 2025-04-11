#!/bin/bash

readonly url="$1"
readonly model="$2"
readonly base_url="$3"

set -ex
readonly tmp="$(mktemp)"
cat > "$tmp"  # instructions from stdin
cat <<EOS >> "$tmp"
--------------------below is the content of ${url}--------------------
EOS
curl -s -L "$url" | markitdown >> "$tmp"

cmd="${DOTFILES_ROOT}/bin/my-ai-agent.sh --debug --model ${model}"
if [ -n "$base_url" ] ; then
    cmd="${cmd} --base_url ${base_url}"
fi
$cmd < "$tmp"
