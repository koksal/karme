#!/bin/bash

OUTFOLDER=$1

for k in {5..20}
do
  FOLDER=$OUTFOLDER-$k-clusters
  ./scripts/run-dendritic-base-args.sh $FOLDER \
    --cell-activity-threshold 0.1 \
    --smoothing-radius 10 \
    --clustering-method kmeans \
    --clustering-distance euclidean \
    --clustering-index kl \
    --plot-cluster-curves \
    --min-clusters $k \
    --max-clusters $k
done
