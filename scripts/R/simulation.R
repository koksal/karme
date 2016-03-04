options(warn = 1)
library(polynom)

args = commandArgs(trailingOnly = TRUE)
outputFolder = args[[1]]

# create subdir for cluster output
dir.create(outputFolder)

# pick measurement times
minT = 0
maxT = 10
stepT = 1

speedCoefStdDev = 0.5
measurementNoiseStdDev = 5

# pick sensible parameters for instantiating polynomials
generatePolynomial <- function(degree) {
  zeros = sample(minT:maxT, degree)
  a = sample(seq(from = -10, to = 10, by = 0.1), 1)
  b = sample(seq(from = -50, to = 50, by = 0.1), 1)
  return(polynomial(coef = c(b, a)))
}

nbPolynomials = 11
degree = 1
generateValuesWithNoise <- function(ps) {
  nbCellsPerMeasurement = 100
  measurementTimes = seq(from = minT, to = maxT, by = stepT)

  originalData = matrix(ncol = length(ps) + 1, nrow = 0)
  observedData = matrix(ncol = length(ps) + 1, nrow = 0)

  for (t in measurementTimes) {
    for (c in 1:nbCellsPerMeasurement) {
      # give this cell a stochastic time value
      speedCoef = rnorm(1, mean = 0, sd = speedCoefStdDev)
      actualTime = max(0, speedCoef * t)
      actualValues = lapply(ps, function(p) predict(p, actualTime))

      # add measurement noise
      noisyValues  = lapply(actualValues, function(v) v + rnorm(1, mean = 0, sd = measurementNoiseStdDev))

      originalData = rbind(originalData, c(actualTime, actualValues))
      observedData = rbind(observedData, c(t, noisyValues))
    }
  }

  colNames = c("t", 1:length(ps))
  origDF = as.data.frame(originalData)
  # names(origDF) = colNames
  obsDF  = as.data.frame(observedData)
  # names(obsDF) = colNames

  write.table(
              originalData, 
              file = paste(outputFolder, "/original.tsv", sep = ""),
              sep = "\t", 
              row.names = FALSE, 
              col.names = FALSE
              )
  write.table(
              observedData, 
              file = paste(outputFolder, "/observed.tsv", sep = ""),
              sep = "\t", 
              row.names = FALSE, 
              col.names = FALSE
              )
}

ps = lapply(1:nbPolynomials, function(i) generatePolynomial(degree))
generateValuesWithNoise(ps)
