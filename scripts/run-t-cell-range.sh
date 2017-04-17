#!/bin/bash

NB_JOBS=8

set -o xtrace

function run_with_args() {
  folder="log/parallel_runs/run_"`echo $@ | sed s'/[\ \/-]/_/g'`
  echo $folder
  scripts/run-t-cell-base-args.sh $folder \
    --pseudolog-factor 2 \
    --boolean-normalization mclust \
    --max-hamming 1 \
    --max-expr-depth 3 \
    "$@"
}

# export function so parallel can access it
export -f run_with_args

declare -A TRANSFORM_ARGS
ACTIVITY_FILTER_ARGS["none"]=""
ACTIVITY_FILTER_ARGS["pseudolog-2"]="--pseudolog-factor 2"

declare -A ACTIVITY_FILTER_ARGS
ACTIVITY_FILTER_ARGS["05"]="--cell-activity-threshold 0.05"
ACTIVITY_FILTER_ARGS["10"]="--cell-activity-threshold 0.1"
ACTIVITY_FILTER_ARGS["20"]="--cell-activity-threshold 0.2"

declare -A UNCERTAINTY_ARGS
UNCERTAINTY_ARGS["0.3"]="--uncertainty-threshold 0.3"
UNCERTAINTY_ARGS["0.4"]="--uncertainty-threshold 0.4"
UNCERTAINTY_ARGS["0.5"]="--uncertainty-threshold 0.5"

declare -A SMOOTHING_RADIUS_ARGS
SMOOTHING_RADIUS_ARGS["15"]="--smoothing-radius 15"
SMOOTHING_RADIUS_ARGS["20"]="--smoothing-radius 20"

# clustering
declare -A CLUSTERING_ARGS
CLUSTERING_ARGS["8"]="--cluster --min-clusters 8 --max-clusters 8"
CLUSTERING_ARGS["10"]="--cluster --min-clusters 10 --max-clusters 10"
CLUSTERING_ARGS["12"]="--cluster --min-clusters 12 --max-clusters 12"
CLUSTERING_ARGS["14"]="--cluster --min-clusters 14 --max-clusters 14"

SHELL="/bin/bash" parallel --jobs $NB_JOBS --delay 30 run_with_args \
  ::: "${TRANSFORM_ARGS[@]}" \
  ::: "${ACTIVITY_FILTER_ARGS[@]}" \
  ::: "${UNCERTAINTY_ARGS[@]}" \
  ::: "${SMOOTHING_RADIUS_ARGS[@]}" \
  ::: "${CLUSTERING_ARGS[@]}"
