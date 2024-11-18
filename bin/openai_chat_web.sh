#!/bin/bash

set -e

log="${EMACSD}/history_openai-chat-web"
loc="${DOTFILES_ROOT}/ivg/repos/openai-chat-web"
command="pipenv run python -m openai_chat_web.cli $*"

write_log() {
    local log_input_file="$1"
    local log_output_file="$2"
    local log_err_file="$3"

    local ts
    ts="$(date +%s)"
    local now
    now="$(date -r "$ts" "+%Y-%m-%d %H:%M:%S")"
    jq -n -c \
       --arg c "$command" \
       --arg i "$(cat "$log_input_file")" \
       --arg o "$(cat "$log_output_file")" \
       --arg e "$(cat "$log_err_file")" \
       --arg ts "$ts" \
       --arg now "$now" \
       '{input: $i, output: $o, err: $e, ts: $ts, now: $now, command: $c}' >> "$log"
}

main() {
    local input
    input="$(mktemp)"
    local output
    output="$(mktemp)"
    local err
    err="$(mktemp)"
    cd "$loc"
    tee "$input" | $command > "$output" 2> "$err"
    cat "$output"
    cat "$err" >&2
    if [ -e "$(which jq)" ] ; then
        write_log "$input" "$output" "$err"
    fi
}

main
