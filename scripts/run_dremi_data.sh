#!/bin/bash

LABEL="dremi-data"

if [ "$1" != "" ]; then
  LABEL=$1
fi

./scripts/run_experiment_with_default_args.sh \
  data/dremi/CD4_naive_series1_CD3_CD28.csv \
  data/dremi/names/all.txt \
  $LABEL
