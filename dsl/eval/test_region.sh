#!/usr/bin/env bash

set -uo pipefail

CSV="$PWD/results.csv"
CLOVE_DIR="$PWD/output"
REPEATS=5
SBT="sbt.bat"

REGION_COUNTS=(1 10 50 100 250 500 1000 2000 5000 10000)
ENTITIES=(1 5 10 25 50)
REGIONS=1000


run_config() { # $1 = regions, $2 = entities
  local m=$1 n=$2
  $SBT "runMain testRegions --regions $m --entities $n"
  for r in $(seq 1 $((REPEATS-1))); do
    love "$CLOVE_DIR"
  done
}

for m in "${REGION_COUNTS[@]}"; do
  echo ">> M=$m  (x$REPEATS)"
  run_config "$m" 1
done

for n in "${ENTITIES[@]}"; do
  echo ">> N=$n (M=$REGIONS)  (x$REPEATS)"
  run_config "$REGIONS" "$n"
done