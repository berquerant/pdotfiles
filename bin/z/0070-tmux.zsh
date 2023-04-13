#!/bin/zsh

export TMUX_LAUNCH_PREFIX="tmux_launch"
alias tmux='tmux -2'

tmux_launch() {
    local now=`date "+%Y-%m-%d_%H:%M:%S"`
    local uid=`uuidgen`
    local name="${TMUX_LAUNCH_PREFIX}_${now}_${uid}"
    tmux new-window -n $name $@
}

alias tns='tmux new -s'
alias tls='tmux ls'
alias tlsc='tmux lsc'
alias ta='tmux a -t'
alias tks='tmux kill-session -t'
alias tkill='tmux kill-server'
