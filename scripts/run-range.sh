#!/bin/bash

set -o xtrace

function run_with_args() {
  folder="log/parallel_runs/run_"`echo $@ | sed s'/[\ \/-]/_/g'`
  echo $folder
  scripts/run-default.sh $folder \
    --annotations data/names/markers.txt \
    --uncertainty 0.0 \
    --cluster \
    --minClust 2 \
    --maxClust 30 \
    --curves \
    --graphs \
    "$@"
}

# export function so parallel can access it
export -f run_with_args

# filtering genes by name
declare -A FILTER_NAMES_ARGS
# FILTER_NAMES_ARGS["none"]=""
# FILTER_NAMES_ARGS["markers"]="--names data/names/markers.txt"
# FILTER_NAMES_ARGS["human-tf"]="--names data/names/human-tf.txt"
# FILTER_NAMES_ARGS["animaltfdb"]="--names data/names/animaltfdb-mus-musculus.txt"
FILTER_NAMES_ARGS["markers-and-human-tf"]="--names data/names/markers.txt,data/names/human-tf.txt"
FILTER_NAMES_ARGS["markers-and-animaltfdb"]="--names data/names/markers.txt,data/names/animaltfdb-mus-musculus.txt"

# data transformation
declare -A DATA_TRANSFORM_ARGS
DATA_TRANSFORM_ARGS["none"]=""
DATA_TRANSFORM_ARGS["pseudolog-2"]="--pseudolog-factor 2"
# DATA_TRANSFORM_ARGS["pseudolog-5"]="--pseudolog-factor 5"

# filtering genes by active cell ratio
declare -A ACTIVITY_FILTER_ARGS
# ACTIVITY_FILTER_ARGS["none"]=""
# ACTIVITY_FILTER_ARGS["10-percent"]="--activity-ratio 0.1"
ACTIVITY_FILTER_ARGS["20-percent"]="--activity-ratio 0.2"
# ACTIVITY_FILTER_ARGS["30-percent"]="--activity-ratio 0.3"

# force include annotations in data
declare -A FORCE_ANNOTATION_ARGS
FORCE_ANNOTATION_ARGS["no"]=""
FORCE_ANNOTATION_ARGS["yes"]="--force-annotations"

# first discretization
declare -A FIRST_DISCRETIZATION_ARGS
FIRST_DISCRETIZATION_ARGS["kmeans"]="--first-discretization kmeans"
FIRST_DISCRETIZATION_ARGS["mclust"]="--first-discretization mclust"

# smoothing
declare -A SMOOTHING_RADIUS_ARGS
# SMOOTHING_RADIUS_ARGS["none"]="--smoothing-radius 0"
# SMOOTHING_RADIUS_ARGS["10"]="--smoothing-radius 10"
SMOOTHING_RADIUS_ARGS["20"]="--smoothing-radius 20"
# SMOOTHING_RADIUS_ARGS["30"]="--smoothing-radius 30"

SHELL="/bin/bash" parallel --jobs 4 --delay 30 run_with_args \
  ::: "${FILTER_NAMES_ARGS[@]}" \
  ::: "${DATA_TRANSFORM_ARGS[@]}" \
  ::: "${ACTIVITY_FILTER_ARGS[@]}" \
  ::: "${FORCE_ANNOTATION_ARGS[@]}" \
  ::: "${FIRST_DISCRETIZATION_ARGS[@]}" \
  ::: "${SMOOTHING_RADIUS_ARGS[@]}"
