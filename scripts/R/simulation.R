options(warn = 1)
library(polynom)

# pick time bins
nbTimeBins = 10
minX = 0
maxX = 100
timeBinSize = (maxX - minX) / nbTimeBins

# pick sensible parameters for instantiating polynomials
generatePolynomial <- function(degree) {
  minCoef = -10
  maxCoef = 10
  coefs = sample(minCoef:maxCoef, degree)
  return(polynomial(coef = coefs))
}

nbPolynomials = 5
degree = 3
generateValuesWithNoise <- function(ps) {
  nbPoints = 10000
  step = (maxX - minX) / nbPoints
  xValues = seq(from = minX, to = maxX, by = step)

  originalData = matrix(ncol = length(ps) + 1, nrow = 0)
  observedData = matrix(ncol = length(ps) + 1, nrow = 0)

  for (x in xValues) {
    f <- function(p) {
      return(predict(p, x))
    }
    actualValues = lapply(ps, f)
    # add measurement noise
    noisyValues  = lapply(actualValues, function(v) v + rnorm(1))

    # add time noise and compute noisy bin
    timeNoise = rnorm(1)
    noisyTime = x + timeNoise
    timeBin = noisyTime %/% timeBinSize

    originalData = rbind(originalData, c(x, actualValues))
    observedData = rbind(observedData, c(timeBin, noisyValues))
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
