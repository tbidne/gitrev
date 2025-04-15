#!/usr/bin/env bash

# This script intended to be used by CI.

set -e

cmd="cabal run example -- "

# All four should success for cabal build.

out_legacy=$($cmd hash_legacy)
if [[ -z $out_legacy ]]; then
  echo "*** hash_legacy: received unexpected empty ***"
  exit 1
elif [[ $out_legacy == 'UNKNOWN' ]]; then
  echo "*** hash_legacy: received unexpected UNKNOWN ***"
  exit 1
else
  echo "*** hash_legacy: received $out_legacy ***"
fi

out_env_val=$($cmd hash_env_val)
if [[ -z $out_env_val ]]; then
  echo "*** hash_env_val: received unexpected empty ***"
  exit 1
elif [[ $out_env_val == 'UNKNOWN' ]]; then
  echo "*** hash_env_val: received unexpected UNKNOWN ***"
  exit 1
else
  echo "*** hash_env_val: received $out_env_val ***"
fi

out_env_dir=$($cmd hash_env_dir)
if [[ -z $out_env_dir ]]; then
  echo "*** hash_env_dir: received unexpected empty ***"
  exit 1
elif [[ $out_env_dir == 'UNKNOWN' ]]; then
  echo "*** hash_env_dir: received unexpected UNKNOWN ***"
  exit 1
else
  echo "*** hash_env_dir: received $out_env_dir ***"
fi

out_env_val_dir=$($cmd hash_env_val_dir)
if [[ -z $out_env_val_dir ]]; then
  echo "*** hash_env_val_dir: received unexpected empty ***"
  exit 1
elif [[ $out_env_val_dir == 'UNKNOWN' ]]; then
  echo "*** hash_env_val_dir: received unexpected UNKNOWN ***"
  exit 1
else
  echo "*** hash_env_val_dir: received $out_env_val_dir ***"
fi
