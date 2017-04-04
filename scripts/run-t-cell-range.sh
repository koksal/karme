#!/bin/bash

NB_JOBS=8

set -o xtrace

function run_with_args() {
  folder="log/parallel_runs/run_"`echo $@ | sed s'/[\ \/-]/_/g'`
  echo $folder
  scripts/run-t-cell-base-args.sh $folder \
    --synthesis \
    --pseudolog-factor 2 \
    --max-hamming 1 \
    "$@"
}

# export function so parallel can access it
export -f run_with_args

# boolean normalization
declare -A NORMALIZATION_ARGS
NORMALIZATION_ARGS["kmeans"]="--boolean-normalization kmeans"
NORMALIZATION_ARGS["mclust"]="--boolean-normalization mclust"

# filtering genes by active cell ratio
declare -A ACTIVITY_FILTER_ARGS
ACTIVITY_FILTER_ARGS["10"]="--cell-activity-threshold 0.1"
ACTIVITY_FILTER_ARGS["20"]="--cell-activity-threshold 0.2"

# uncertainty
declare -A UNCERTAINTY_ARGS
UNCERTAINTY_ARGS["0.3"]="--uncertainty-threshold 0.3"
UNCERTAINTY_ARGS["0.4"]="--uncertainty-threshold 0.4"
UNCERTAINTY_ARGS["0.5"]="--uncertainty-threshold 0.5"

# smoothing
declare -A SMOOTHING_RADIUS_ARGS
SMOOTHING_RADIUS_ARGS["10"]="--smoothing-radius 10"
SMOOTHING_RADIUS_ARGS["20"]="--smoothing-radius 20"

# clustering
declare -A CLUSTERING_ARGS
CLUSTERING_ARGS["8"]="--cluster --min-clusters 8 --max-clusters 8"
CLUSTERING_ARGS["10"]="--cluster --min-clusters 10 --max-clusters 10"
CLUSTERING_ARGS["12"]="--cluster --min-clusters 12 --max-clusters 12"

# max expr depth
declare -A EXPR_DEPTH_ARGS
EXPR_DEPTH_ARGS["2"]="--max-expr-depth 2"
EXPR_DEPTH_ARGS["3"]="--max-expr-depth 3"
EXPR_DEPTH_ARGS["4"]="--max-expr-depth 4"

SHELL="/bin/bash" parallel --jobs $NB_JOBS --delay 30 run_with_args \
  ::: "${NORMALIZATION_ARGS[@]}" \
  ::: "${ACTIVITY_FILTER_ARGS[@]}" \
  ::: "${UNCERTAINTY_ARGS[@]}" \
  ::: "${SMOOTHING_RADIUS_ARGS[@]}" \
  ::: "${CLUSTERING_ARGS[@]}" \
  ::: "${EXPR_DEPTH_ARGS[@]}"
