#!/bin/bash

NB_JOBS=8

set -o xtrace

function run_with_args() {
  folder="log/parallel_runs/run_"`echo $@ | sed s'/[\ \/-]/_/g'`
  echo $folder
  scripts/run-olfactory-base-args.sh $folder \
    --annotations data/names/markers.txt \
    --boolean-normalization mclust \
    --uncertainty-threshold 1 \
    --synthesis \
    --max-expr-depth 1 \
    "$@"
}

# export function so parallel can access it
export -f run_with_args

declare -A TRANSFORM_ARGS
for i in 2 10
do
  TRANSFORM_ARGS["pseudolog-$i"]="--pseudolog-factor $i"
done

declare -A ACTIVITY_FILTER_ARGS
for i in 0.1 0.2
do
  ACTIVITY_FILTER_ARGS["$i"]="--cell-activity-threshold $i"
done

declare -A SMOOTHING_RADIUS_ARGS
for i in 10 15 20
do
  SMOOTHING_RADIUS_ARGS["$i"]="--smoothing-radius $i"
done

# clustering
declare -A CLUSTERING_ARGS
for i in {10..15}
do 
  CLUSTERING_ARGS["$i"]="--cluster --min-clusters $i --max-clusters $i"
done

SHELL="/bin/bash" parallel --jobs $NB_JOBS --delay 60 --joblog joblog.txt run_with_args \
  ::: "${TRANSFORM_ARGS[@]}" \
  ::: "${ACTIVITY_FILTER_ARGS[@]}" \
  ::: "${SMOOTHING_RADIUS_ARGS[@]}" \
  ::: "${CLUSTERING_ARGS[@]}"
