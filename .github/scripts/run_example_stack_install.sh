#!/usr/bin/env bash

set -e

cmd="./build/example "
legacy_success="true"
env_val_success="true"
env_dir_success="true"

# Stack install works for all three yay.

.github/scripts/run_example.sh \
  "$cmd" \
  $legacy_success \
  $env_val_success \
  $env_dir_success
