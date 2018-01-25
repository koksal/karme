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

for f in $PARENT_OUTPUT_FOLDER/resolution/p=*
do
  ./scripts/aggregate-tables.sh \
    $f/stable-state-reachability-wildtype.tsv \
    $f/replicate-*/stable-state-reachability-wildtype.tsv
done

./scripts/aggregate-boxplot.sh \
  $PARENT_OUTPUT_FOLDER/resolution/ssrwt \
  $PARENT_OUTPUT_FOLDER/resolution/p=*/stable-state-reachability-wildtype.tsv

# Latexify all tables
# ./scripts/latexify-table.sh \
#   $PARENT_OUTPUT_FOLDER/cell-trajectory-quality/sigma=*/stable-state-reachability-wildtype.tsv
