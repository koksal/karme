#!/bin/bash

OUTFOLDER=$1
SWEEP_FOLDER=evaluation/2017-05-21-parameter-sweep-with-clustering-and-reachability

./scripts/run-t-cell-eval.sh \
  $OUTFOLDER/function-io-pairs \
  --prediction-type fun-io-pairs \
  --run-collection $SWEEP_FOLDER

for i in 1..5
do
  ./scripts/run-t-cell-eval.sh \
    $OUTFOLDER/precedence-up-to-$i \
    --prediction-type precedence-pairs \
    --max-precedence-distance $i \
    --run-collection $SWEEP_FOLDER
done
