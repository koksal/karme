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

# Model behavior: Fixed point states for knockouts
./scripts/aggregate-tables.sh \
  $PARENT_OUTPUT_FOLDER/behavior-eval \
  $PARENT_OUTPUT_FOLDER/*/behavior-eval.tsv

./scripts/latexify-table.sh \
  $PARENT_OUTPUT_FOLDER/behavior-eval-all.tsv

# Model behavior: Fixed point states for perturbations
./scripts/aggregate-tables.sh \
  $PARENT_OUTPUT_FOLDER/hidden-vs-inferred-models-perturbed-state-fixpoints \
  $PARENT_OUTPUT_FOLDER/*/hidden-vs-inferred-models-perturbed-state-fixpoints.tsv

./scripts/latexify-table.sh \
  $PARENT_OUTPUT_FOLDER/hidden-vs-inferred-models-perturbed-state-fixpoints-all.tsv

# Function similarity
./scripts/aggregate-tables.sh \
  $PARENT_OUTPUT_FOLDER/function-similarity-eval \
  $PARENT_OUTPUT_FOLDER/*/function-similarity-eval.tsv

./scripts/latexify-table.sh \
  $PARENT_OUTPUT_FOLDER/function-similarity-eval-all.tsv
