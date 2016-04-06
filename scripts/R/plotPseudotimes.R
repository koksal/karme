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

plotPCA <- function(d, proteins) {
  protData = d[,proteins]
  actualTime = d[,"ActualTime"]
  dataPCA = prcomp(protData, center = TRUE, scale. = TRUE)$x
  df = data.frame(
                  x = dataPCA[,1],
                  y = dataPCA[,2],
                  t = actualTime
  )

  p <- ggplot() +
    geom_point(
               data = df,
               aes(x = x, y = y, colour = t),
               )

  fname = paste(outputFolder, "/pca.pdf", sep = "")
  ggsave(file = fname)
}

plotProteinProgression <- function(d, prot) {
  # order by pseudotime and compute moving average
  sortedD <- d[with(d, order(Pseudotime)),]
  orderedProtValues = sortedD[,prot]
  orderedPseudotimes = sortedD[,"Pseudotime"]
  orderedActualTimes = sortedD[,"ActualTime"]
  orderedLogPseudotimes = log(sapply(orderedPseudotimes, function(v) v + exp(1)), base = exp(1))
  orderedLogActualTimes = log(sapply(orderedActualTimes, function(v) v + exp(1)), base = exp(1))
  movingAvg = SMA(orderedProtValues, n = movingAvgWindowSize)

  # compute sample mean values
  minutes = uniqueMinutes(d)
  means = c()
  for (minute in minutes) {
    stepD = subset(d, Minute == minute)
    stepV = stepD[,prot]
    means = append(means, mean(stepV))
  }

  timeDF = data.frame(
    pseudotime = orderedPseudotimes, 
    logPseudotime = orderedLogPseudotimes,
    value = movingAvg, 
    actualTime = orderedActualTimes,
    logActualTime = orderedLogActualTimes
  )
  meanDataFrame = data.frame(x = minutes, y = means)

  # plot values vs. pseudotime, colored by actual time
  p <- ggplot() + 
    geom_point(
               data = timeDF, 
               aes(x = pseudotime, y = value, colour = logActualTime)
               ) + 
    scale_colour_gradient(low = "green", high = "red") + 
    geom_point(
               data = meanDataFrame, 
               aes(x = x, y = y), 
               color = "blue"
               ) # + 
    # scale_x_continuous(trans = log_trans(base = exp(1)))

  fname = paste(outputFolder, "/progression-", prot, ".pdf", sep = "") 
  ggsave(file=fname)

  # this is being computed redundantly right now:
  # plot pseudotime vs. actual time
  p <- ggplot() +
    geom_point(
               data = timeDF,
               aes(x = logActualTime, y = logPseudotime)
               )

  fname = paste(outputFolder, "/pseudotime-vs-actualtime.pdf", sep = "")
  ggsave(file=fname)
}

plotProgressions <- function() {
  d = readData(inputDataFile)

  plotPCA(d, proteins)

  for (prot in proteins) {
    plotProteinProgression(d, prot)
  }
}

plotProgressions()
