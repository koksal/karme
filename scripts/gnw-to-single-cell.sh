#!/bin/bash

INPUTPREFIX=$1
OUTFILE=single-cell.csv

INITHEAP="50G"
MAXHEAP="50G"
STACK="1G"

sbt -J-Xmx${INITHEAP} -J-Xms${MAXHEAP} -J-Xss${STACK} "run-main karme.GNW2SingleCells $INPUTPREFIX $OUTFILE"
