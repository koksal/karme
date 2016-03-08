options(warn = 1)
library(polynom)

args = commandArgs(trailingOnly = TRUE)
inputProteinFile  = args[[1]]
seed              = as.integer(args[[2]])
outputFolder      = args[[3]]

# create subdir for cluster output
dir.create(outputFolder)

proteins = readLines(inputProteinFile)

# pick measurement times
minT = 0
maxT = 10
stepT = 1

speedCoefStdDev = 0.5
measurementNoiseStdDev = 5

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

  colNames = c("Minute", proteins)
  colnames(originalData) = colNames
  colnames(observedData) = colNames

  write.table(
              originalData, 
              file = paste(outputFolder, "/original.csv", sep = ""),
              sep = ",", 
              row.names = FALSE, 
              col.names = TRUE
              )
  write.table(
              observedData, 
              file = paste(outputFolder, "/observed.csv", sep = ""),
              sep = ",", 
              row.names = FALSE, 
              col.names = TRUE
              )
}

# ignore degree for now.
generatePolynomial <- function(degree) {
  # pick sensible parameters for instantiating polynomials
  a = sample(seq(from = -10, to = 10, by = 0.1), 1)
  b = sample(seq(from = -50, to = 50, by = 0.1), 1)
  return(polynomial(coef = c(b, a)))
}

generateCurves <- function(nbCurves, degree) {
  lapply(1:nbPolynomials, function(i) generatePolynomial(degree))
}

nbPolynomials = length(proteins)
degree = 1

set.seed(seed)
ps = generateCurves(nbPolynomials, degree)
generateValuesWithNoise(ps)
