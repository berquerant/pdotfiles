#!/bin/zsh

export STEPPING_STONE=stepping-stone

# ssh -L LOCAL_PORT:TARGET_IP:TARGET_PORT STONE_IP
# LOCAL-{port 22}-STONE-{port TARGET_PORT}->TARGET
forward-through-stone() {
    ssh -L "$1":"$2":"$3" $STEPPING_STONE
}

# ssh -R STONE_PORT:TARGET_IP:TARGET_PORT STONE_IP
# LOCAL-{port STONE_PORT}-STONE-{port TARGET_PORT}->TARGET
forward-through-stone-r() {
    ssh -R "$1":"$2":"$3" $STEPPING_STONE
}

export PORT_FORWARD_RECEIVE=10500

forward-through-stone-fixed() {
    forward-through-stone "$PORT_FORWARD_RECEIVE" "$1" "$2"
}
