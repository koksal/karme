options(warn = 1)
library(polynom)

# pick measurement times
minT = 0
maxT = 10
stepT = 1

# pick sensible parameters for instantiating polynomials
generatePolynomial <- function(degree) {
  zeros = sample(minT:maxT, degree)
  a = sample(seq(from = -2, to = 2, by = 0.1), 1)
  b = sample(seq(from = 0, to = 10, by = 0.1), 1)
  return(polynomial(coef = c(b, a)))
}

nbPolynomials = 11
degree = 1
generateValuesWithNoise <- function(ps) {
  nbCellsPerMeasurement = 1000
  measurementTimes = seq(from = minT, to = maxT, by = stepT)

  originalData = matrix(ncol = length(ps) + 1, nrow = 0)
  observedData = matrix(ncol = length(ps) + 1, nrow = 0)

  for (t in measurementTimes) {
    for (c in 1:nbCellsPerMeasurement) {
      # give this cell a stochastic time value
      speedCoef = rnorm(1, mean = 1, sd = 1)
      actualTime = max(speedCoef * t, 0)
      actualValues = lapply(ps, function(p) predict(p, actualTime))

      # add measurement noise
      noisyValues  = lapply(actualValues, function(v) v + rnorm(1, mean = 0, sd = 1))

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
              file = "original.tsv", 
              sep = "\t", 
              row.names = FALSE, 
              col.names = FALSE
              )
  write.table(
              observedData, 
              file = "observed.tsv", 
              sep = "\t", 
              row.names = FALSE, 
              col.names = FALSE
              )
}

ps = lapply(1:nbPolynomials, function(i) generatePolynomial(degree))
generateValuesWithNoise(ps)
