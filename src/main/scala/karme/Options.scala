package karme

import java.io.File

case class Options(
  continuousExperimentFile: Option[File] = None,
  discreteExperimentFile: Option[File] = None,
  clusterFile: Option[File] = None,
  namesFile: Option[File] = None,
  trajectoryFiles: Seq[File] = Seq(),
  outFolder: File = new File("."),
  visualize: Boolean = false,
  discreteAnalysis: Boolean = false,
  continuousAnalysis: Boolean = false,
  analysisOptions: AnalysisOptions = AnalysisOptions()
)

case class AnalysisOptions(
  windowRadius: Int = 20
)
