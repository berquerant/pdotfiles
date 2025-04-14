#!/bin/zsh

export EXTERNAL_AI_MODEL="gpt-4o-mini"
export INTERNAL_AI_MODEL="gemma3:4b"
export OLLAMA_PORT="11434"
export OLLAMA_HOST="127.0.0.1:${OLLAMA_PORT}"
export OPEN_WEBUI_PORT="3000"
export MCPO_PORT="8000"
export OPEN_WEBUI_URL="http://localhost:${OPEN_WEBUI_PORT}"
export MCPO_URL="http://localhost:${MCPO_PORT}/docs"
export OPEN_WEBUI_VOLUMES_ROOT="${DOTFILES_ROOT}/tmp/open_webui/volumes"
mkdir -p "${OPEN_WEBUI_VOLUMES_ROOT}"
ai-open() {
    open-webui up -d
    "${DOTFILES_ROOT}/bin/healthcheck.sh" "${MCPO_URL}" &&\
        "${DOTFILES_ROOT}/bin/healthcheck.sh" "${OPEN_WEBUI_URL}" || return 1
    open "${MCPO_URL}"
    open "${OPEN_WEBUI_URL}"
}
readonly obsidian_vault="${HOME}/Documents/Obsidian Vault"
readonly obsidian_vault_link="${DOTFILES_ROOT}/tmp/obsidian/vault"
mkdir -p "$(dirname "$obsidian_vault_link")" "${obsidian_vault}"
ln -sf "${obsidian_vault}" "${obsidian_vault_link}"
