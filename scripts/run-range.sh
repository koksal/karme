#!/bin/bash

set -o xtrace

function run_with_args() {
  folder="log/parallel_runs/run_"`echo $@ | sed s'/[\ \/-]/_/g'`
  echo $folder
  scripts/run-with-base-args.sh $folder \
    --annotations data/names/markers.txt \
    --synthesis \
    --uncertainty-threshold 0.4 \
    --max-expr-depth 5 \
    --evaluation-libraries data/reference/enrichr/human-tf/ChEA_2016-enrichr-predictions.csv,data/reference/enrichr/human-tf/ENCODE_TF_ChIP-seq_2015-enrichr-predictions.csv,data/reference/enrichr/human-tf/ENCODE_and_ChEA_Consensus_TFs_from_ChIP-X-enrichr-predictions.csv,data/reference/enrichr/human-tf/TF-LOF_Expression_from_GEO-enrichr-predictions.csv,data/reference/enrichr/human-tf/TRANSFAC_and_JASPAR_PWMs-enrichr-predictions.csv \
    "$@"
}

# export function so parallel can access it
export -f run_with_args

# filtering genes by name
declare -A FILTER_NAMES_ARGS
# FILTER_NAMES_ARGS["markers"]="--names data/names/markers.txt"
FILTER_NAMES_ARGS["human-tf"]="--names data/names/human-tf.txt"

# data transformation
declare -A DATA_TRANSFORM_ARGS
# DATA_TRANSFORM_ARGS["none"]=""
DATA_TRANSFORM_ARGS["pseudolog-2"]="--pseudolog-factor 2"

# filtering genes by active cell ratio
declare -A ACTIVITY_FILTER_ARGS
# ACTIVITY_FILTER_ARGS["none"]=""
ACTIVITY_FILTER_ARGS["20-percent"]="--cell-activity-threshold 0.2"

# boolean normalization
declare -A BOOLEAN_NORMALIZATION_ARGS
BOOLEAN_NORMALIZATION_ARGS["kmeans"]="--boolean-normalization kmeans"
# BOOLEAN_NORMALIZATION_ARGS["mclust"]="--boolean-normalization mclust"

# smoothing
declare -A SMOOTHING_RADIUS_ARGS
# SMOOTHING_RADIUS_ARGS["none"]="--smoothing-radius 0"
# SMOOTHING_RADIUS_ARGS["10"]="--smoothing-radius 10"
SMOOTHING_RADIUS_ARGS["20"]="--smoothing-radius 20"
# SMOOTHING_RADIUS_ARGS["30"]="--smoothing-radius 30"

# clustering
declare -A CLUSTERING_ARGS
# CLUSTERING_ARGS["10"]="--cluster --min-clusters 10 --max-clusters 10"
CLUSTERING_ARGS["12"]="--cluster --min-clusters 12 --max-clusters 12"
CLUSTERING_ARGS["14"]="--cluster --min-clusters 14 --max-clusters 14"
CLUSTERING_ARGS["16"]="--cluster --min-clusters 16 --max-clusters 16"
CLUSTERING_ARGS["range"]="--cluster --min-clusters 10 --max-clusters 20"
# CLUSTERING_ARGS["18"]="--cluster --min-clusters 18 --max-clusters 18"
# CLUSTERING_ARGS["20"]="--cluster --min-clusters 20 --max-clusters 20"

SHELL="/bin/bash" parallel --jobs 5 --delay 30 run_with_args \
  ::: "${FILTER_NAMES_ARGS[@]}" \
  ::: "${DATA_TRANSFORM_ARGS[@]}" \
  ::: "${ACTIVITY_FILTER_ARGS[@]}" \
  ::: "${BOOLEAN_NORMALIZATION_ARGS[@]}" \
  ::: "${SMOOTHING_RADIUS_ARGS[@]}" \
  ::: "${CLUSTERING_ARGS[@]}"
