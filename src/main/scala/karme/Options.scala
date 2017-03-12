package karme

import java.io.File

import karme.discretization.Ckmeans
import karme.discretization.DiscretizationMethod

case class Options(
  // input files
  continuousExperimentFile: Option[File] = None,
  discretizedExperimentFile: Option[File] = None,
  mleExperimentFile: Option[File] = None,
  clusterFile: Option[File] = None,
  namesFiles: Seq[File] = Seq(),
  annotationsFiles: Seq[File] = Seq(),
  trajectoryFiles: Seq[File] = Seq(),
  // output
  outFolder: File = new File("."),
  // pipeline options
  runSynthesis: Boolean = false,
  runSimulation: Boolean = false,
  // sub-options
  analysisOptions: AnalysisOptions = AnalysisOptions(),
  visualizationOptions: VisualizationOptions = VisualizationOptions()
)

case class AnalysisOptions(
  pseudoLogFactor: Option[Double] = None,
  firstDiscretizationMethod: DiscretizationMethod = Ckmeans,
  cellActivityThreshold: Double = 0.20,
  forceAnnotations: Boolean = false,
  uncertaintyMargin: Double = 0.05,
  smoothingRadius: Int = 20,
  maxHammingDistance: Int = 1,
  cluster: Boolean = false,
  minNbClusters: Int = 2,
  maxNbClusters: Int = 30
)

case class VisualizationOptions(
  histograms: Boolean = false,
  boxPlots: Boolean = false,
  curves: Boolean = false,
  stateGraphs: Boolean = false
)
