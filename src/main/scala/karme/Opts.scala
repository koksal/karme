package karme

import java.io.File

import karme.evaluation.PredictionTypes.{FunIOPairsPrediction, PredictionType}
import karme.transformations.discretization.{DiscretizationMethod, Thresholding}

case class Opts(
  inputFileOpts: InputFileOpts = InputFileOpts(),
  inputTransformerOpts: InputTransformerOpts = InputTransformerOpts(),
  synthOpts: SynthOpts = SynthOpts(),
  evalOpts: EvalOpts = EvalOpts(),
  annotationOpts: AnnotationOpts = AnnotationOpts(),
  reporterOpts: ReporterOpts = ReporterOpts(),
  runPrecedence: Boolean = false,
  runSynthesis: Boolean = false
)

case class InputTransformerOpts(
  pseudoLogFactor: Option[Double] = None,
  booleanNormalizationMethod: DiscretizationMethod = Thresholding,
  minDifferentialThreshold: Double = 0.20,
  uncertaintyThreshold: Double = 1,
  smoothingRadius: Int = 20,
  maxHammingDistance: Int = 1,
  clusteringOpts: ClusteringOpts = ClusteringOpts(),
  distributionComparisonMethod: String = "ranksum",
  distributionComparisonPValue: Double = 0.05,
  plotOriginalData: Boolean = false,
  plotTransformedData: Boolean = false,
  plotBinarizedData: Boolean = false,
  plotBinarizedGeneCurves: Boolean = false,
  plotSmoothedGeneCurves: Boolean = false,
  plotClusterCurves: Boolean = false,
  plotThreeValuedData: Boolean = false
)

case class InputFileOpts(
  continuousExperimentFile: Option[File] = None,
  discretizedExperimentFile: Option[File] = None,
  smoothedExperimentFile: Option[File] = None,
  namesFiles: Seq[File] = Seq(),
  trajectoryFiles: Seq[File] = Seq(),
  knockdownExperimentFiles: Seq[File] = Seq()
)

case class ClusteringOpts(
  cluster: Boolean = false,
  minNbClusters: Int = 2,
  maxNbClusters: Int = 30,
  clusteringIndex: String = "gap",
  clusteringMethod: String = "ward.D2",
  clusteringDistance: String = "euclidean",
  refineClusters: Boolean = false,
  clusterRefinementPValue: Double = 0.05
)

case class AnnotationOpts(
  cellClusteringFile: Option[File] = None,
  annotationsFiles: Seq[File] = Seq()
)

case class SynthOpts(
  maxExpressionDepth: Int = 3,
  maxNbModels: Option[Int] = None
)

case class EvalOpts(
  perturbationTargetsFile: Option[File] = None,
  expectedDriversFile: Option[File] = None,
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
