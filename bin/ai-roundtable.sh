#!/bin/bash

# usage:
# echo "SOME_AGENDA" | bash bin/ai-roundtable.sh start -c SOME_CONFIG.yml
# cat "SOME_THREAD" | bash bin/ai-roundtable.sh rerun -c SOME_CONFIG.yml
#

readonly loc="${DOTFILES_ROOT}/ivg/repos/ai-roundtable"
readonly eval_out="$(mktemp)"

run() {
    cd "$loc" || return 1
    pipenv run python -m ai_roundtable.cli "$@" --eval_out "$eval_out"
    cat "$eval_out" >&2
}

start() {
    run "$@" --agenda "@-"
}

rerun() {
    local -r old_thread="$(mktemp)"
    cat - > "$old_thread"
    cat "$old_thread"
    run "$@" --thread "-" < "$old_thread"
}

readonly cmd="$1"
shift
case "$cmd" in
    start) start "$@" ;;
    rerun) rerun "$@" ;;
    *)
        echo "Unknown command: sub=${cmd} args=$*" >&2
        exit 1
        ;;
esac
sleep 2
