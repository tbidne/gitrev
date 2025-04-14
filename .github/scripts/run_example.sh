#!/usr/bin/env bash

# This script intended to be used by CI.

set -e

# should be 'cabal' or 'nix'
build_type=$1

if [[ $build_type == 'cabal' ]]; then
  cmd="cabal run example -- "
elif [[ $build_type == 'nix' ]]; then
  cmd="./result/bin/example "
else
  echo "*** Received unexpected command: $build_type ***"
  exit 1
fi

out_legacy=$($cmd hash_legacy)
# 1. Output is empty, always a failure.
if [[ -z $out_legacy ]]; then
  echo "*** hash_legacy: received empty ***"
  exit 1
else
  if [[ $build_type == 'cabal' ]]; then
    # 2.1 Cabal received "UNKNOWN", unexpected (failure).
    if [[ $out_legacy == 'UNKNOWN' ]]; then
      echo "***hash_legacy (cabal): expected success, received UNKNOWN ***"
      exit 1
    # 2.2 Cabal received other non-empty string (success).
    else
      echo "***hash_legacy (cabal): received $out_legacy ***"
    fi
  else
    # 3.1 Nix received "UNKNOWN"; this is expected (success)
    if [[ $out_legacy == 'UNKNOWN' ]]; then
      echo "***hash_legacy (nix): received UNKNOWN ***"
    # 3.2 Nix received other non-empty string, unexpected success (failure).
    else
      echo "***hash_legacy (nix): expected UNKNOWN, received $out_legacy ***"
      exit 1
    fi
  fi
fi

# Unlike legacy above, this should work for both cabal and nix, since we
# set the env var in our flake.
out_typed=$($cmd hash_typed)
if [[ -z $out_typed ]]; then
  echo "*** hash_typed: received empty ***"
  exit 1
elif [[ $out_typed == "UNKNOWN" ]]; then
  echo "*** hash_typed: received UNKNOWN ***"
  exit 1
else
  echo "*** hash_typed: $out_typed ***"
fi
