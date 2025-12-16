#!/bin/bash

golangci-lint run \
              --output.checkstyle.path=stdout \
              --output.text.path=stderr \
              --path-mode=abs \
              . 2>/dev/null
