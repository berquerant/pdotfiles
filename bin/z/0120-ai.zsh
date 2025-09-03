#!/bin/zsh

export OLLAMA_PORT="11434"
export OLLAMA_HOST="127.0.0.1:${OLLAMA_PORT}"
export OPEN_WEBUI_PORT="3000"
export MCPO_PORT="8000"
export OPEN_WEBUI_URL="http://localhost:${OPEN_WEBUI_PORT}"
export MCPO_URL="http://localhost:${MCPO_PORT}/docs"
export OPEN_WEBUI_VOLUMES_ROOT="${DOTFILES_ROOT}/tmp/open_webui/volumes"
mkdir -p "${OPEN_WEBUI_VOLUMES_ROOT}"
ai-open() {
    echo 'NOTE: ollama on host is required'
    open-webui up
    "${DOTFILES_ROOT}/bin/healthcheck.sh" "${OPEN_WEBUI_URL}" || return 1
    open "${OPEN_WEBUI_URL}"
}
