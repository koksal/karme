#!/bin/bash

OUTFOLDER=$1
SWEEP_FOLDER=evaluation/2017-06-16-parameter-sweep

./scripts/run-t-cell-eval.sh \
  $OUTFOLDER/function-io-pairs-non-normalized-scores \
  --prediction-type fun-io-pairs \
  --run-collection $SWEEP_FOLDER

./scripts/run-t-cell-eval.sh \
  $OUTFOLDER/function-io-pairs-normalized-scores \
  --prediction-type fun-io-pairs \
  --normalize-scores \
  --run-collection $SWEEP_FOLDER

for i in {1..5}
do
  ./scripts/run-t-cell-eval.sh \
    $OUTFOLDER/randomized-predictions-$i \
    --prediction-type fun-io-pairs \
    --randomize-eval \
    --run-collection $SWEEP_FOLDER
done
