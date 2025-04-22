#!/usr/bin/env bash

set -e

cmd="./build/example "
legacy_success="false"
env_val_success="false"
env_dir_success="true"

# Only tests w/ env dir should succeed for cabal install

.github/scripts/run_example.sh \
  "$cmd" \
  $legacy_success \
  $env_val_success \
  $env_dir_success
