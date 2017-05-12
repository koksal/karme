#!/bin/bash

NB_JOBS=8

set -o xtrace

function run_with_args() {
  folder="log/parallel_runs/"`echo $@ | sed s'/[\ \/-]/_/g'`
  echo $folder
  scripts/run-t-cell-base-args.sh $folder \
    --boolean-normalization mclust \
    "$@"
}

# export function so parallel can access it
export -f run_with_args

declare -A TRANSFORM_ARGS
TRANSFORM_ARGS["pseudolog2"]="--pseudolog-factor 2"

declare -A ACTIVITY_FILTER_ARGS
ACTIVITY_FILTER_ARGS["none"]="--cell-activity-threshold 0"
ACTIVITY_FILTER_ARGS["20"]="--cell-activity-threshold 0.2"

declare -A UNCERTAINTY_ARGS
UNCERTAINTY_ARGS["1.0"]="--uncertainty-threshold 1"

declare -A SMOOTHING_RADIUS_ARGS
SMOOTHING_RADIUS_ARGS["0"]="--smoothing-radius 0"
SMOOTHING_RADIUS_ARGS["10"]="--smoothing-radius 10"
SMOOTHING_RADIUS_ARGS["20"]="--smoothing-radius 20"

# clustering
declare -A CLUSTERING_ARGS
CLUSTERING_ARGS["8"]="--cluster --min-clusters 8 --max-clusters 8"
CLUSTERING_ARGS["9"]="--cluster --min-clusters 8 --max-clusters 9"
CLUSTERING_ARGS["10"]="--cluster --min-clusters 10 --max-clusters 10"
CLUSTERING_ARGS["11"]="--cluster --min-clusters 10 --max-clusters 11"
CLUSTERING_ARGS["12"]="--cluster --min-clusters 12 --max-clusters 12"

SHELL="/bin/bash" parallel --jobs $NB_JOBS --delay 10 run_with_args \
  ::: "${TRANSFORM_ARGS[@]}" \
  ::: "${ACTIVITY_FILTER_ARGS[@]}" \
  ::: "${UNCERTAINTY_ARGS[@]}" \
  ::: "${SMOOTHING_RADIUS_ARGS[@]}" \
  ::: "${CLUSTERING_ARGS[@]}"
