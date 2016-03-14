options(warn = 1)
library(ggplot2)
library(TTR)
library(scales)

args = commandArgs(trailingOnly = TRUE)
inputDataFile     = args[[1]]
inputProteinFile  = args[[2]]
outputFolder      = args[[3]]

movingAvgWindowSize = 1

# create subdir for cluster output
dir.create(outputFolder)

proteins = readLines(inputProteinFile)

readData <- function(fn) {
  d = read.csv(fn, check.names = FALSE)
  return(d)
}

uniqueSteps <- function(d) {
  stepCol = d[, "Step"]
  return(unique(stepCol))
}

uniqueMinutes <- function(d) {
  minCol = d[, "Minute"]
  return(unique(minCol))
}

plotPseudotimeHistogram <- function(d, step) {
  d = d[d$Step %in% c(step), ]
  pts = d[,"Pseudotime"]

  p <- qplot(pts, geom="histogram")
  fname = paste(outputFolder, "/", "pseudotime-histogram-step-", step, ".pdf", sep = "")
  ggsave(file=fname)
}

plotHistograms <- function() {
  d = readData(inputDataFile)

  for (step in uniqueSteps(d)) {
    plotPseudotimeHistogram(d, step)
  }
}

plotProteinProgression <- function(d, prot) {
  # order by pseudotime and compute moving average
  sortedD <- d[with(d, order(Pseudotime)),]
  orderedProtValues = sortedD[,prot]
  orderedPseudotimes = sortedD[,"Pseudotime"]
  movingAvg = SMA(orderedProtValues, n = movingAvgWindowSize)

  # compute sample mean values
  minutes = uniqueMinutes(d)
  means = c()
  for (minute in minutes) {
    stepD = subset(d, Minute == minute)
    stepV = stepD[,prot]
    means = append(means, mean(stepV))
  }

  pseudoTimeDataFrame = data.frame(x = orderedPseudotimes, y = movingAvg)
  meanDataFrame = data.frame(x = minutes, y = means)

  p <- ggplot() + 
    geom_point(
               data = pseudoTimeDataFrame, 
               aes(x = x, y = y), 
               color = "grey60", 
               shape=1
               ) + 
    geom_point(
               data = meanDataFrame, 
               aes(x = x, y = y), 
               color = "orangered3", 
               fill = "orangered3", 
               shape = 21
               ) + 
    scale_x_continuous(trans = log_trans(base = exp(1)))

  fname = paste(outputFolder, "/progression-", prot, ".pdf", sep = "") 
  ggsave(file=fname)
}

plotProgressions <- function() {
  d = readData(inputDataFile)

  for (prot in proteins) {
    plotProteinProgression(d, prot)
  }
}

plotProgressions()
