#!/usr/bin/env bash

set -uo pipefail

CSV="$PWD/results.csv"
CLOVE_DIR="$PWD/output"
REPEATS=5
SBT="sbt.bat"

DEPTHS=(1 2 4 8 16 32 64)
MODES=(region_plain region_prop entity_plain entity_prop)

run_config() { # $1 = mode, $2 = depth
  local mode=$1 d=$2
  $SBT "runMain testStack --depth $d --mode $mode"
  for r in $(seq 1 $((REPEATS-1))); do
    love "$CLOVE_DIR"
  done
}

for mode in "${MODES[@]}"; do
  for d in "${DEPTHS[@]}"; do
    echo ">> mode=$mode depth=$d  (x$REPEATS)"
    run_config "$mode" "$d"
  done
done