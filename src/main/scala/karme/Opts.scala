package karme

import java.io.File

import karme.transformations.discretization.DiscretizationMethod
import karme.transformations.discretization.Thresholding

case class Opts(
  inputTransformerOpts: InputTransformerOpts = InputTransformerOpts(),
  synthOpts: SynthOpts = SynthOpts(),
  evalOpts: EvalOpts = EvalOpts(),
  annotationOpts: AnnotationOpts = AnnotationOpts(),
  reporterOpts: ReporterOpts = ReporterOpts(),
  runPrecedence: Boolean = false,
  runSynthesis: Boolean = false
)

case class InputTransformerOpts(
  inputFileOpts: InputFileOpts = InputFileOpts(),
  pseudoLogFactor: Option[Double] = None,
  booleanNormalizationMethod: DiscretizationMethod = Thresholding,
  minDifferentialThreshold: Double = 0.20,
  uncertaintyThreshold: Double = 1,
  smoothingRadius: Int = 20,
  maxHammingDistance: Int = 1,
  clusteringOpts: ClusteringOpts = ClusteringOpts(),
  refineClusters: Boolean = false,
  clusterRefinementPValue: Double = 0.05,
  plotOriginalData: Boolean = false,
  plotTransformedData: Boolean = false,
  plotBinarizedData: Boolean = false,
  plotBinarizedGeneCurves: Boolean = false,
  plotSmoothedGeneCurves: Boolean = false,
  plotClusterCurves: Boolean = false,
  plotBinarizedClusterData: Boolean = false
)

case class InputFileOpts(
  continuousExperimentFile: Option[File] = None,
  discretizedExperimentFile: Option[File] = None,
  smoothedExperimentFile: Option[File] = None,
  namesFiles: Seq[File] = Seq(),
  trajectoryFiles: Seq[File] = Seq()
)

case class ClusteringOpts(
  minNbClusters: Int = 2,
  maxNbClusters: Int = 30,
  clusteringIndex: String = "gap",
  clusteringMethod: String = "ward.D2",
  clusteringDistance: String = "euclidean"
)

case class AnnotationOpts(
  cellClusteringFile: Option[File] = None,
  annotationsFiles: Seq[File] = Seq()
)

case class SynthOpts(
  maxExpressionDepth: Int = 2,
  maxNbModels: Option[Int] = None
)

sealed trait PredictionType
case object FunIOPairsPrediction extends PredictionType
case object PrecedencePairsPrediction extends PredictionType

case class EvalOpts(
  referenceFiles: Seq[File] = Seq(),
  predictionPairsFile: Option[File] = None,
  runCollectionFolder: Option[File] = None,
  predictionType: PredictionType = FunIOPairsPrediction,
  maxPrecedenceDistance: Option[Int] = None,
  normalizeScores: Boolean = false,
  randomize: Boolean = false
)

case class ReporterOpts(
  outFolder: File = new File("."),
  verbose: Boolean = false
)
