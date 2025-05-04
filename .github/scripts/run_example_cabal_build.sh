#!/usr/bin/env bash

set -e

cmd="cabal run example -v0 -- "
legacy_success="true"
env_val_success="true"
env_dir_success="true"

# All three should succeed for cabal build.

.github/scripts/run_example.sh \
  "$cmd" \
  $legacy_success \
  $env_val_success \
  $env_dir_success
