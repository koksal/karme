#!/bin/bash

if [ $# -eq 0 ]
then
  echo "Parent output folder not given."
  exit 1
else
  PARENT_OUTPUT_FOLDER=$1
fi

# for f in $PARENT_OUTPUT_FOLDER/cell-trajectory-quality/sigma=*
# do
#   ./scripts/aggregate-tables.sh \
#     $f/stable-state-reachability-wildtype.tsv \
#     $f/replicate-*/stable-state-reachability-wildtype.tsv
# done
# 
# ./scripts/aggregate-boxplot.sh \
#   $PARENT_OUTPUT_FOLDER/cell-trajectory-quality/ssrwt \
#   $PARENT_OUTPUT_FOLDER/cell-trajectory-quality/sigma=*/stable-state-reachability-wildtype.tsv

FILES=(
  "stable-state-reachability-wildtype.tsv" 
  "stable-state-reachability-knockouts.tsv"
  "perturbed-graph-nodes.tsv"
)

for FILE in ${FILES[*]}
do
  for FOLDER in $PARENT_OUTPUT_FOLDER/resolution/p=*
  do
    ./scripts/aggregate-tables.sh \
      $FOLDER/$FILE \
      $FOLDER/replicate-*/$FILE
  done

  ./scripts/aggregate-boxplot.sh \
    $PARENT_OUTPUT_FOLDER/resolution/$FILE \
    $PARENT_OUTPUT_FOLDER/resolution/p=*/$FILE
done

# Latexify all tables
# ./scripts/latexify-table.sh \
#   $PARENT_OUTPUT_FOLDER/cell-trajectory-quality/sigma=*/stable-state-reachability-wildtype.tsv
