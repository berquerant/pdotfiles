#!/bin/bash

gawk --file - --pretty-print=- | sed 's/\t/  /g'
