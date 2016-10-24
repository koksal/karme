package karme

import java.io.File

case class Options(
  continuousExperimentFile: Option[File] = None,
  discreteExperimentFile: Option[File] = None,
  clusterFile: Option[File] = None,
  namesFile: Option[File] = None,
  outFolder: File = new File("."),
  discretize: Boolean = false,
  visualize: Boolean = false,
  discreteAnalysis: Boolean = false,
  continuousAnalysis: Boolean = false
)
