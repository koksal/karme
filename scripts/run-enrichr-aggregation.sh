#!/bin/bash

sbt "run-main karme.evaluation.enrichr.EnrichrPredictionAggregator \
  $*"
