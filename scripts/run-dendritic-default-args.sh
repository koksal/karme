#!/bin/bash

OUTFOLDER_BASE=$1

for DIST_COMP in "ranksum" "ks"
do
  FOLDER=$OUTFOLDER_BASE/$DIST_COMP
  ./scripts/run-dendritic-base-args.sh $FOLDER \
    --distribution-comparison $DIST_COMP
done
