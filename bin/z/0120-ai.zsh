#!/bin/zsh

export OLLAMA_PORT="11434"
export OLLAMA_HOST="127.0.0.1:${OLLAMA_PORT}"
export AI_AGENT_CHECKPOINT_LOG_ROOT="${TMPD}/ai-agent-checkpoint"
