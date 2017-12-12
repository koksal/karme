#!/bin/bash

if [ $# -eq 0 ]
then
  echo "Output folder not given."
  exit 1
else
  OUTFOLDER_BASE=$1
fi

./scripts/run-synthetic-workflow.sh $OUTFOLDER_BASE-oracle --partial-order oracle

for distcomp in ranksum ks
do
  for p in 0.1 0.2 0.5
  do
    ./scripts/run-synthetic-workflow.sh $OUTFOLDER_BASE-$distcomp-p-$p --partial-order comparison --distribution-comparison $distcomp --distribution-comparison-p-value $p
  done
done

for distcomp in average minimum
do
    ./scripts/run-synthetic-workflow.sh $OUTFOLDER_BASE-$distcomp --partial-order comparison --distribution-comparison $distcomp
done
