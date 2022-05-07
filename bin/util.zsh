#!/bin/zsh

# find files or directories >= 5GB
diskcheck() {
    sudo du -m -x -d 5 / 2> /dev/null | awk '$1 >= 5000' | awk '{print $1, length($2), $2}' | sort -rn | awk '{print $1, $3}'
}
