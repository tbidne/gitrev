#!/usr/bin/env bash

set -e

cmd="./result/bin/example "
legacy_success="false"
env_val_success="true"
env_dir_success="false"

# Only tests w/ env val should succeed for nix.

.github/scripts/run_example.sh \
  "$cmd" \
  $legacy_success \
  $env_val_success \
  $env_dir_success
