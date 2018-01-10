#!/bin/bash

if [ $# -eq 0 ]
then
  echo "Parent output folder not given."
  exit 1
else
  PARENT_OUTPUT_FOLDER=$1
fi

# Graph diff
./scripts/aggregate-tables.sh \
  $PARENT_OUTPUT_FOLDER/graph-diff.tsv \
  $PARENT_OUTPUT_FOLDER/*/graph-diff.tsv

# Model behavior: Stable states for knockouts
./scripts/aggregate-tables.sh \
  $PARENT_OUTPUT_FOLDER/stable-state-reachability-across-conditions.tsv \
  $PARENT_OUTPUT_FOLDER/*/stable-state-reachability-across-conditions.tsv

# Model behavior: Stable states for perturbations
./scripts/aggregate-tables.sh \
  $PARENT_OUTPUT_FOLDER/stable-state-reachability-across-environments-median.tsv \
  $PARENT_OUTPUT_FOLDER/*/stable-state-reachability-across-environments-median.tsv

# Function similarity
./scripts/aggregate-tables.sh \
  $PARENT_OUTPUT_FOLDER/function-similarity-median.tsv \
  $PARENT_OUTPUT_FOLDER/*/function-similarity-median.tsv

# Latexify all tables
./scripts/latexify-table.sh \
  $PARENT_OUTPUT_FOLDER/graph-diff.tsv \
  $PARENT_OUTPUT_FOLDER/stable-state-reachability-across-conditions.tsv \
  $PARENT_OUTPUT_FOLDER/stable-state-reachability-across-environments-median.tsv \
  $PARENT_OUTPUT_FOLDER/*/stable-state-reachability-across-environments-*.tsv \
  $PARENT_OUTPUT_FOLDER/function-similarity-median.tsv \
  $PARENT_OUTPUT_FOLDER/*/function-similarity-*.tsv \
  $PARENT_OUTPUT_FOLDER/*/synthesis-times-in-milliseconds.tsv


