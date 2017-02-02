package karme

import java.io.File

case class Options(
  continuousExperimentFile: Option[File] = None,
  discretizedExperimentFile: Option[File] = None,
  mleExperimentFile: Option[File] = None,
  clusterFile: Option[File] = None,
  namesFiles: Seq[File] = Seq(),
  trajectoryFiles: Seq[File] = Seq(),
  outFolder: File = new File("."),
  discreteAnalysis: Boolean = false,
  continuousAnalysis: Boolean = false,
  analysisOptions: AnalysisOptions = AnalysisOptions(),
  visualizationOptions: VisualizationOptions = VisualizationOptions()
)

case class AnalysisOptions(
  windowRadius: Int = 20,
  maxHammingDistance: Int = 1,
  nbClusters: Int = 30
)

case class VisualizationOptions(
  histograms: Boolean = false,
  boxPlots: Boolean = false,
  curves: Boolean = false,
  stateGraph: Boolean = false
)
