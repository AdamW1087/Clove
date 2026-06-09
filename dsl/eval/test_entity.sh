#!/usr/bin/env bash

set -uo pipefail

COUNTS=(50 100 250 500 1000 2000 4000 5000 6000 8000)
REPEATS=5
CSV="$PWD/results.csv"

ENTITY_DIR="$PWD/run_entity"
ENTITY_DIR_MIN="$PWD/run_entity_min"
CLOVE_DIR="$PWD/output"
SBT="sbt.bat"

rm -f "$CSV"
mkdir -p "$ENTITY_DIR" "$ENTITY_DIR_MIN"
cp ./entity_scale.lua         "$ENTITY_DIR/main.lua"
cp ./entity_scale_min.lua     "$ENTITY_DIR_MIN/main.lua"

run_clove_generate() { # $1 = count. change test here for regular and minimal
  local n=$1
  $SBT "runMain testMin --entities $n"
}

run_clove_play() {
  love "$CLOVE_DIR"
}

run_lua() { # $1 = dir, $2 = count
  local dir=$1 n=$2
  ENTITY_COUNT=$n CSV="$CSV" love "$dir" >/dev/null 2>&1
}

for n in "${COUNTS[@]}"; do
  for r in $(seq 1 "$REPEATS"); do
    run_lua "$ENTITY_DIR_MIN" "$n"
  done

  run_clove_generate "$n"

  for r in $(seq 1 $((REPEATS-1))); do
    run_clove_play
  done
done