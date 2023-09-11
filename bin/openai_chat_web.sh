#!/bin/bash

set -e

log="${EMACSD}/history_openai-chat-web"
repo="berquerant/openai-chat-web"
loc="${GHQ_ROOT}/github.com/${repo}"
command="pipenv run python -m openai_chat_web.cli $@"

repo_exist() {
    ghq list | grep -q "$repo"
}

ensure_init_repo() {
    if repo_exist ; then
        return
    fi

    ghq get "$repo"
    cd "$loc"
    pipenv install
}

write_log() {
    log_input_file="$1"
    log_output_file="$2"

    ts=$(date +%s)
    now=$(date -r "$ts" "+%Y-%m-%d %H:%M:%S")
    jq -n -c \
       --arg c "$command" \
       --arg i "$(cat "$log_input_file")" \
       --arg o "$(cat "$log_output_file")" \
       --arg ts "$ts" \
       --arg now "$now" \
       '{input: $i, output: $o, ts: $ts, now: $now, command: $c}' >> "$log"
}

main() {
    ensure_init_repo
    input=$(mktemp)
    output=$(mktemp)
    cd "$loc"
    tee "$input" | $command | tee "$output"
    if [ -e "$(which jq)" ] ; then
        write_log "$input" "$output"
    fi
}

main
