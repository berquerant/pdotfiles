#!/bin/bash

grep -E '^[a-zA-Z_-]+:.*?## .*$' | sort |  awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $1, $2}'
