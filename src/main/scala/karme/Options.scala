package karme

import java.io.File

case class Options(
  // input files
  continuousExperimentFile: Option[File] = None,
  discretizedExperimentFile: Option[File] = None,
  mleExperimentFile: Option[File] = None,
  clusterFile: Option[File] = None,
  namesFiles: Seq[File] = Seq(),
  annotationsFile: Option[File] = None,
  trajectoryFiles: Seq[File] = Seq(),
  // output
  outFolder: File = new File("."),
  // pipeline options
  runElbow: Boolean = false,
  runSynthesis: Boolean = false,
  runSimulation: Boolean = false,
  // sub-options
  analysisOptions: AnalysisOptions = AnalysisOptions(),
  visualizationOptions: VisualizationOptions = VisualizationOptions()
)

case class AnalysisOptions(
  cellActivityThreshold: Double = 0.20,
  windowRadius: Int = 20,
  maxHammingDistance: Int = 1,
  nbClusters: Option[Int] = None
)

case class VisualizationOptions(
  histograms: Boolean = false,
  boxPlots: Boolean = false,
  curves: Boolean = false,
  stateGraph: Boolean = false
)
