#!/usr/local/bin/bash

set -o xtrace

function run_with_args() {
  folder="run_"`echo $@ | sed s'/[\ \/-]/_/g'`
  echo $folder
  scripts/run-default.sh $folder "$@" &
}

# export function so parallel can access it
export -f run_with_args

# filtering genes by name
declare -A FILTER_NAMES_ARGS
FILTER_NAMES_ARGS["none"]=""
FILTER_NAMES_ARGS["markers"]="--names data/names/markers.txt"
FILTER_NAMES_ARGS["human-tf"]="--names data/names/human-tf.txt"
FILTER_NAMES_ARGS["animaltfdb"]="--names data/names/animaltfdb-mus-musculus.txt"
FILTER_NAMES_ARGS["markers-and-human-tf"]="--names data/names/markers.txt,data/names/human-tf.txt"
FILTER_NAMES_ARGS["markers-and-animaltfdb"]="--names data/names/markers.txt,data/names/animaltfdb-mus-musculus.txt"

# data transformation
declare -A DATA_TRANSFORM_ARGS
DATA_TRANSFORM_ARGS["none"]=""
DATA_TRANSFORM_ARGS["pseudolog-2"]="--pseudolog-factor 2"
DATA_TRANSFORM_ARGS["pseudolog-5"]="--pseudolog-factor 5"

# filtering genes by active cell ratio
declare -A ACTIVITY_FILTER_ARGS
# ACTIVITY_FILTER_ARGS["none"]=""
ACTIVITY_FILTER_ARGS["10-percent"]="--activity-ratio 0.1"
ACTIVITY_FILTER_ARGS["20-percent"]="--activity-ratio 0.2"
ACTIVITY_FILTER_ARGS["30-percent"]="--activity-ratio 0.3"

parallel --files run_with_args \
  ::: "--bogus argument"
  # ::: "${FILTER_NAMES_ARGS[@]}"
