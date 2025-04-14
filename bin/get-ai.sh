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
"${DOTFILES_ROOT}/bin/url2markdown.sh" "$url" >> "$tmp"

cmd="${DOTFILES_ROOT}/bin/my-ai-agent.sh --debug --model ${model}"
if [ -n "$base_url" ] ; then
    cmd="${cmd} --base_url ${base_url}"
fi
$cmd < "$tmp"
