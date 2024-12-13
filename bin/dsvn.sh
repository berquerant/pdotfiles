#!/bin/bash

svn "$@" | nkf -w
