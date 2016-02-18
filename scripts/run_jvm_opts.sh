#!/bin/bash

INITHEAP="10G"
MAXHEAP="10G"
STACK="1G"

sbt -J-Xmx${INITHEAP} -J-Xms${MAXHEAP} -J-Xss${STACK} "run-main karme.Main $*"
