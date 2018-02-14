#!/bin/bash

if [ $# -eq 0 ]
then
  echo "Parent output folder not given."
  exit 1
else
  PARENT_OUTPUT_FOLDER=$1
fi

FILES=(
  "stable-states-wildtype.tsv" 
  "stable-states-knockouts.tsv"
  "reachable-states-wildtype.tsv" 
  "reachable-states-knockouts.tsv"
  "sampled-states.tsv"
  "reconstructed-states.tsv"
  "function-similarity.tsv"
)

# for EVAL_TYPE in "noise" "resolution" "noise-and-resolution"
for EVAL_TYPE in "noise-and-resolution"
do
  for FILE in ${FILES[*]}
  do
    for FOLDER in $PARENT_OUTPUT_FOLDER/$EVAL_TYPE/*=*
    do
      ./scripts/aggregate-tables.sh \
        $FOLDER/$FILE \
        $FOLDER/replicate-*/$FILE

    done

    if [ $EVAL_TYPE = "noise-and-resolution" ]
    then
      ./scripts/aggregate-heatmap.sh \
        $PARENT_OUTPUT_FOLDER/$EVAL_TYPE/$FILE \
        $PARENT_OUTPUT_FOLDER/$EVAL_TYPE/*/$FILE
    else
      ./scripts/aggregate-boxplot.sh \
        $PARENT_OUTPUT_FOLDER/$EVAL_TYPE/$FILE \
        $PARENT_OUTPUT_FOLDER/$EVAL_TYPE/*/$FILE
    fi
  done
done

# Latexify all tables
# ./scripts/latexify-table.sh \
#   $PARENT_OUTPUT_FOLDER/cell-trajectory-quality/sigma=*/stable-state-reachability-wildtype.tsv
