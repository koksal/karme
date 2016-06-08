package karme

case class Options(
  proteinNamesPath:       java.io.File = null,
  experimentPath:         java.io.File = null,
  simulate:               Boolean = false,
  evaluate:               Boolean = false,
  speedCoefSD:            Double = 1,
  noiseSD:                Double = 1,
  outLabel:               Option[String] = None,
  outFolder:              java.io.File = new java.io.File("."),
  filterPositive:         Boolean = false,
  sampleCount:            Option[Int] = None,
  seed:                   Option[Int] = None,
  maxTime:                Option[Double] = None,
  arcsinhFactor:          Option[Double] = None,
  labelPropagationOpts:   LabelPropagationOptions = LabelPropagationOptions()
)

case class LabelPropagationOptions(
  alpha:                Double = 0.5,
  nbNeighbors:          Int = 5,
  nbIter:               Int = 10,
  timeWeight:           Double = 1.0,
  useJaccardSimilarity: Boolean = false
)
