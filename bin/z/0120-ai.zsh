#!/bin/zsh

export EXTERNAL_AI_MODEL="gpt-4o-mini"
export INTERNAL_AI_MODEL="gemma3:4b"
export INTERNAL_ANALYSIS_AI_MODEL="gemma3:4b"
export OLLAMA_PORT="11434"
export OLLAMA_HOST="127.0.0.1:${OLLAMA_PORT}"
export OPEN_WEBUI_PORT="3000"
export MCPO_PORT="8000"
ai-local() {
    open-webui up -d
    open "http://localhost:${OPEN_WEBUI_PORT}"
    open "http://localhost:${MCPO_PORT}/docs"
}
