#!/bin/bash

if [ $# -eq 0 ]
then
  echo "Parent output folder not given."
  exit 1
else
  PARENT_OUTPUT_FOLDER=$1
fi

# # Graph diff
./scripts/aggregate-tables.sh \
  $PARENT_OUTPUT_FOLDER/graph-diff \
  $PARENT_OUTPUT_FOLDER/*/graph-diff.tsv

./scripts/latexify-table.sh \
  $PARENT_OUTPUT_FOLDER/graph-diff-median.tsv

# Model behavior: Stable states for knockouts
./scripts/aggregate-tables.sh \
  $PARENT_OUTPUT_FOLDER/stable-state-reachability-across-conditions \
  $PARENT_OUTPUT_FOLDER/*/stable-state-reachability-across-conditions.tsv

./scripts/latexify-table.sh \
  $PARENT_OUTPUT_FOLDER/stable-state-reachability-across-conditions-all.tsv

# Model behavior: Stable states for perturbations
./scripts/aggregate-tables.sh \
  $PARENT_OUTPUT_FOLDER/stable-state-reachability-across-environments \
  $PARENT_OUTPUT_FOLDER/*/hidden-vs-inferred-models-perturbed-state-fixpoints-*.tsv

./scripts/latexify-table.sh \
  $PARENT_OUTPUT_FOLDER/stable-state-reachability-across-environments-all.tsv \
  $PARENT_OUTPUT_FOLDER/*/hidden-vs-inferred-models-perturbed-state-fixpoints-*.tsv

# Function similarity
./scripts/aggregate-tables.sh \
  $PARENT_OUTPUT_FOLDER/function-similarity-eval \
  $PARENT_OUTPUT_FOLDER/*/function-similarity-*.tsv

./scripts/latexify-table.sh \
  $PARENT_OUTPUT_FOLDER/function-similarity-eval-all.tsv \
  $PARENT_OUTPUT_FOLDER/*/function-similarity-*.tsv
