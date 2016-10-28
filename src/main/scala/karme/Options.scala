package karme

import java.io.File

case class Options(
  continuousExperimentFile: Option[File] = None,
  discreteExperimentFile: Option[File] = None,
  clusterFile: Option[File] = None,
  namesFile: Option[File] = None,
  trajectoryFiles: Set[File] = Set(),
  outFolder: File = new File("."),
  visualize: Boolean = false,
  discreteAnalysis: Boolean = false,
  continuousAnalysis: Boolean = false
)
