#!/bin/bash

LABEL="simulated-data"

if [ "$1" != "" ]; then
  LABEL=$1
fi

./scripts/run_experiment_with_default_args.sh \
  data/simulated/gnw-linear-0.20-sde-10k-runs/single-cell.csv \
  data/simulated/names/linear.txt \
  $LABEL
