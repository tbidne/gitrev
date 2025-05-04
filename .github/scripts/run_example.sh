#!/usr/bin/env bash

set -e

CMD=$1
LEGACY_SUCCESS=$2
ENV_VAL_SUCCESS=$3
ENV_DIR_SUCCESS=$4

regex="([a-f0-9]{40})"

# [[:space:]]+ for newlines
complex_regex="Example: [0-9.]+[[:space:]]+ - Git revision: [a-z0-9]{40}[[:space:]]+ - Commit date:  [0-9\-]{10}[[:space:]]+ - GHC version:  [0-9\.]{3,4}"

# 1: command
# 2: arg
# 3: "false" | "true"
run_cmd () {
  cmd=$1
  arg=$2
  expect_success=$3

  out=$($cmd $arg)

  # empty output is always an error
  if [[ -z $out ]]; then
    echo "*** $arg: received unexpected empty ***"
    exit 1
  fi

  # 1. If we are expecting a failure, then it should be UNKNOWN. Anything else
  # is an error.
  if [[ $expect_success == "false" ]]; then
    if [[ $out == 'UNKNOWN' ]]; then
      echo "*** $arg: received expected UNKNOWN ***"
    else
      echo "*** $arg: received unexpected: $out ***"
      exit 1
    fi
  # 2. If we are expected a success, then it should be a git hash.
  elif [[ $expect_success == "true" ]]; then
    if [[ $out =~ $regex ]]; then
      echo "*** $arg: received expected hash: ${BASH_REMATCH[1]} ***"
    else
      echo "*** $arg: received unexpected: $out ***"
      exit 1
    fi
  # 3. Complex example, should always succeed.
  elif [[ $expect_success == "complex" ]]; then
    if [[ $out =~ $complex_regex ]]; then
      echo "*** $arg: received expected complex output: ***"
      echo "$out"
    else
      echo "*** $arg: received unexpected: ***"
      echo "$out"
      exit 1
    fi
  # 4. Expectation failure.
  else
    echo "*** $arg: Wanted (false|true) for expect_success, received: $expect_success ***"
    exit 1
  fi
}

# 1. STRING
echo -e "*** String ***\n"
run_cmd "$CMD" "hash_legacy" $LEGACY_SUCCESS
run_cmd "$CMD" "hash_env_val" $ENV_VAL_SUCCESS
run_cmd "$CMD" "hash_env_dir" $ENV_DIR_SUCCESS

if [[ $LEGACY_SUCCESS == "true" \
   || $ENV_VAL_SUCCESS == "true" \
   || $ENV_DIR_SUCCESS == "true" ]]; then
  any_success="true"
else
  any_success="false"
fi

run_cmd "$CMD" "hash_env_val_dir" $any_success
run_cmd "$CMD" "complex" "complex"

# 2. OSSTRING
echo -e "\n*** OsString ***\n"
run_cmd "$CMD" "hash_legacy_os" $LEGACY_SUCCESS
run_cmd "$CMD" "hash_env_val_os" $ENV_VAL_SUCCESS
run_cmd "$CMD" "hash_env_dir_os" $ENV_DIR_SUCCESS
run_cmd "$CMD" "hash_env_val_dir_os" $any_success
run_cmd "$CMD" "complex_os" "complex"
