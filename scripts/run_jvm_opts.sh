#!/bin/bash

INITHEAP="50G"
MAXHEAP="50G"
STACK="1G"

sbt -J-Xmx${INITHEAP} -J-Xms${MAXHEAP} -J-Xss${STACK} "run-main karme.Main $*"
