#!/bin/bash

GNWFILE=$1
OUTFILE=$GNWFILE-sc.csv
TMP=$(mktemp)

grep -v -e "^$" $1 > $TMP

sbt "run-main karme.GNW2SingleCells $TMP $OUTFILE"

rm $TMP
