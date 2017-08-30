#!/bin/bash

OUTFOLDER=$1

for k in {5..20}
do
  FOLDER=$OUTFOLDER-$k-clusters
  ./scripts/run-dendritic-base-args.sh $FOLDER \
    --boolean-normalization kmeans \
    --cell-activity-threshold 0.1 \
    --smoothing-radius 10 \
    --cluster \
    --clustering-method kmeans \
    --clustering-distance euclidean \
    --clustering-index kl \
    --min-clusters $k \
    --max-clusters $k \
    --plot-binarized-data \
    --plot-smoothed-curves \
    --plot-cluster-curves \
    --plot-three-valued-data
done
