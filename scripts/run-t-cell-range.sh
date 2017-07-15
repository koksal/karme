#!/bin/bash

NB_JOBS=8

set -o xtrace

function run_with_args() {
  folder="log/parallel_runs/"`echo $@ | sed s'/[\ \/-]/_/g'`
  echo $folder
  scripts/run-t-cell-base-args.sh $folder \
    --uncertainty-threshold 1 \
    --refine-clusters \
    --cluster-refinement-p-value 0.01 \
    --synthesis \
    --max-expr-depth 1 \
    "$@"
}

# export function so parallel can access it
export -f run_with_args

declare -A ACTIVITY_FILTER_ARGS
for i in 0.1
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
for i in {5..15}
do 
  CLUSTERING_ARGS["$i"]="--min-clusters $i --max-clusters $i"
done

SHELL="/bin/bash" parallel --jobs $NB_JOBS --delay 60 --joblog joblog.txt run_with_args \
  ::: "${TRANSFORM_ARGS[@]}" \
  ::: "${ACTIVITY_FILTER_ARGS[@]}" \
  ::: "${SMOOTHING_RADIUS_ARGS[@]}" \
  ::: "${CLUSTERING_ARGS[@]}"
