#!/usr/bin/env bash

set -e

cmd="stack run example --cabal-verbosity 0 -- "
legacy_success="true"
env_val_success="true"
env_dir_success="true"

# All three should succeed for stack build.

.github/scripts/run_example.sh \
  "$cmd" \
  $legacy_success \
  $env_val_success \
  $env_dir_success
