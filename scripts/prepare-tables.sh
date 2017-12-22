#!/bin/bash

if [ $# -eq 0 ]
then
  echo "Parent output folder not given."
  exit 1
else
  PARENT_OUTPUT_FOLDER=$1
fi

# Orientation evaluation 
./scripts/aggregate-tables.sh \
  $PARENT_OUTPUT_FOLDER/orientation-eval \
  $PARENT_OUTPUT_FOLDER/*/orientation-eval.tsv

./scripts/latexify-table.sh \
  $PARENT_OUTPUT_FOLDER/orientation-eval-all.tsv

./scripts/latexify-table.sh \
  $PARENT_OUTPUT_FOLDER/orientation-eval-median.tsv
