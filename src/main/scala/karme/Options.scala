package karme

case class Options(
  proteinNamesPath:       java.io.File = null,
  experimentPath:         java.io.File = null,
  outLabel:               Option[String] = None,
  outFolder:              java.io.File = new java.io.File("."),
  filterPositive:         Boolean = false,
  sample:                 Boolean = false,
  maxTime:                Option[Double] = None,
  arcsinhFactor:          Double = 5.0,
  propagationAlpha:       Double = 0.5,
  propagationNbNeighbors: Int = 5,
  propagationNbIter:      Int = 10,
  propagationSplitTime:   Option[Double] = None
)
