#!/bin/bash

git switch "$(git remote show origin | grep -F 'HEAD branch:' | cut -d ':' -f 2 | tr -d ' ')"
