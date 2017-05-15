#!/bin/bash

NB_JOBS=12

set -o xtrace

function run_with_args() {
  folder="log/parallel_runs/"`echo $@ | sed s'/[\ \/-]/_/g'`
  echo $folder
  scripts/run-t-cell-base-args.sh $folder \
    --boolean-normalization mclust \
    --uncertainty-threshold 1 \
    --synthesis \
    --max-expr-depth 1 \
    "$@"
}

# export function so parallel can access it
export -f run_with_args

declare -A TRANSFORM_ARGS
for i in 2 5 10
do
  TRANSFORM_ARGS["pseudolog-$i"]="--pseudolog-factor $i"
done

declare -A ACTIVITY_FILTER_ARGS
for i in 0 0.1 0.2
do
  ACTIVITY_FILTER_ARGS["$i"]="--cell-activity-threshold $i"
done

declare -A SMOOTHING_RADIUS_ARGS
for i in 0 10 20
do
  SMOOTHING_RADIUS_ARGS["$i"]="--smoothing-radius $i"
done

# clustering
declare -A CLUSTERING_ARGS
for i in {5..15}
do 
  CLUSTERING_ARGS["$i"]="--cluster --min-clusters $i --max-clusters $i"
done

SHELL="/bin/bash" parallel --jobs $NB_JOBS --delay 10 run_with_args \
  ::: "${TRANSFORM_ARGS[@]}" \
  ::: "${ACTIVITY_FILTER_ARGS[@]}" \
  ::: "${SMOOTHING_RADIUS_ARGS[@]}" \
  ::: "${CLUSTERING_ARGS[@]}"
