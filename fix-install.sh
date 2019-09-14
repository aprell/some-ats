#!/usr/bin/env bash

set -eu

path=$(dirname "$1")

if [ ! -d "$path/lib64" ]; then
    echo "Creating directory $path/lib64"
    mkdir "$path/lib64"
fi
