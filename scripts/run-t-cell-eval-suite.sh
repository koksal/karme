#!/bin/bash

OUTFOLDER=$1
SWEEP_FOLDER=evaluation/2017-06-06-parallel-runs-small-range

# ./scripts/run-t-cell-eval.sh \
#   $OUTFOLDER/function-io-pairs \
#   --prediction-type fun-io-pairs \
#   --run-collection $SWEEP_FOLDER
# 
for i in {1..3}
do
  ./scripts/run-t-cell-eval.sh \
    $OUTFOLDER/precedence-up-to-$i \
    --prediction-type precedence-pairs \
    --max-precedence-distance $i \
    --normalize-scores \
    --run-collection $SWEEP_FOLDER
done

# for i in {1..5}
# do
#   ./scripts/run-t-cell-eval.sh \
#     $OUTFOLDER/randomized-predictions-$i \
#     --prediction-type fun-io-pairs \
#     --randomize-eval \
#     --run-collection $SWEEP_FOLDER
# done
