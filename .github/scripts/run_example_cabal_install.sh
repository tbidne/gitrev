#!/usr/bin/env bash

# This script intended to be used by CI.

set -e

cmd="./build/example "

# The only examples that should succeed for cabal install are ones that
# lookup the dir by the EXAMPLE_HOME env var i.e. the last two.

out_legacy=$($cmd hash_legacy)
if [[ -z $out_legacy ]]; then
  echo "*** hash_legacy: received unexpected empty ***"
  exit 1
elif [[ $out_legacy == 'UNKNOWN' ]]; then
  echo "*** hash_legacy: received expected UNKNOWN ***"
else
  echo "*** hash_legacy: received unexpected $out_legacy ***"
  exit 1
fi

out_env_val=$($cmd hash_env_val)
if [[ -z $out_env_val ]]; then
  echo "*** hash_env_val: received unexpected empty ***"
  exit 1
elif [[ $out_env_val == 'UNKNOWN' ]]; then
  echo "*** hash_env_val: received expected UNKNOWN ***"
else
  echo "*** hash_env_val: received unexpected $out_env_val ***"
  exit 1
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
