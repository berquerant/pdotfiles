#!/bin/bash

# usage:
# echo "MESSAGE" | bash bin/my-ai-agent.sh
#

readonly loc="${DOTFILES_ROOT}/ivg/repos/my-ai-agent"
cd "$loc" || return 1
pipenv run python -m my_ai_agent.cli "$@"
