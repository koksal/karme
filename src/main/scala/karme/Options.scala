package karme

import java.io.File

case class Options(
  continuousExperimentFile: Option[File] = None,
  clusterFile: Option[File] = None,
  namesFile: Option[File] = None,
  trajectoryFiles: Seq[File] = Seq(),
  outFolder: File = new File("."),
  discreteAnalysis: Boolean = false,
  continuousAnalysis: Boolean = false,
  analysisOptions: AnalysisOptions = AnalysisOptions(),
  visualizationOptions: VisualizationOptions = VisualizationOptions()
)

case class AnalysisOptions(
  windowRadius: Int = 20,
  maxHammingDistance: Int = 1
)

case class VisualizationOptions(
  histograms: Boolean = false,
  boxPlots: Boolean = false,
  curves: Boolean = false,
  stateGraph: Boolean = false
)
