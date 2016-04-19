options(warn = 1)
library(polynom)

args = commandArgs(trailingOnly = TRUE)
inputProteinFile  = args[[1]]
speedCoefSD       = as.double(args[[2]])
noiseSD           = as.double(args[[3]])
seed              = as.integer(args[[4]])
outputFile        = args[[5]]

proteins = readLines(inputProteinFile)

# pick measurement times
nbMeasurements = 10

generateValuesWithNoise <- function(ps) {
  nbCellsPerMeasurement = 500
  measurementTimes = lapply(0:(nbMeasurements - 1), function(x) 2^x)

  observedData = matrix(ncol = length(ps) + 2, nrow = 0)

  for (t in measurementTimes) {
    for (c in 1:nbCellsPerMeasurement) {
      e = exp(1)
      # give this cell a stochastic time value
      speedCoefMean = - (speedCoefSD * speedCoefSD) / 2
      speedCoef = e ^ rnorm(1, mean = speedCoefMean, sd = speedCoefSD)
      actualTime = speedCoef * t
      actualValues = lapply(ps, function(p) predict(p, actualTime))

      # add measurement noise
      noiseMean = - (noiseSD * noiseSD) / 2
      noisyValues  = lapply(actualValues, function(v) v * e ^ (rnorm(1, mean = noiseMean, sd = noiseSD)))

      observedData = rbind(observedData, c(actualTime, t, noisyValues))
    }
  }

  colNames = c("ActualTime", "Minute", proteins)
  colnames(observedData) = colNames

  write.table(
              observedData, 
              file = outputFile,
              sep = ",", 
              row.names = FALSE, 
              col.names = TRUE
              )
}

generatePolynomial <- function(degree) {
  zeros = sample(seq(from = 0, to = 10, by = 0.1), degree)
  sign = sample(c(-1, 1), 1)
  absPoly = poly.calc(zeros)
  amplitude = sample(seq(from = 1, to = 10, by = 0.1), 1)
  p = absPoly * sign * amplitude + 1000
  return(p)
}

generateCurves <- function(nbCurves, degree) {
  lapply(1:nbPolynomials, function(i) generatePolynomial(degree))
}

nbPolynomials = length(proteins)
degree = 2

set.seed(seed)
ps = generateCurves(nbPolynomials, degree)
generateValuesWithNoise(ps)
