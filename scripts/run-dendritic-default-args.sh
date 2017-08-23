#!/bin/bash

OUTFOLDER=$1

for k in 5
do
  FOLDER=$OUTFOLDER-$k-clusters
  ./scripts/run-dendritic-base-args.sh $FOLDER \
    --boolean-normalization kmeans \
    --cell-activity-threshold 0.1 \
    --smoothing-radius 10 \
    --clustering-method kmeans \
    --clustering-distance euclidean \
    --clustering-index kl \
    --min-clusters $k \
    --max-clusters $k
done
