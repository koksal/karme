package karme

import java.io.File

case class Options(
  continuousExperimentFile: Option[File] = None,
  discreteExperimentFile: Option[File] = None,
  clusterFile: Option[File] = None,
  outFolder: Option[File] = None,
  discretize: Boolean = false,
  visualize: Boolean = false,
  analyzeDiscreteStates: Boolean = false,
  analyzeContinuousStates: Boolean = true
)
