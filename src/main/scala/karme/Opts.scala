package karme

import java.io.File

import karme.transformations.discretization.Ckmeans
import karme.transformations.discretization.DiscretizationMethod

case class Opts(
  outFolder: File = new File("."),
  inputTransformerOpts: InputTransformerOpts = InputTransformerOpts(),
  synthOpts: SynthOpts = SynthOpts(),
  evalOpts: EvalOpts = EvalOpts(),
  annotationOpts: AnnotationOpts = AnnotationOpts(),
  runSynthesis: Boolean = false
)

case class InputTransformerOpts(
  inputFileOpts: InputFileOpts = InputFileOpts(),
  pseudoLogFactor: Option[Double] = None,
  booleanNormalizationMethod: DiscretizationMethod = Ckmeans,
  cellActivityThreshold: Double = 0.20,
  uncertaintyThreshold: Double = 0.4,
  smoothingRadius: Int = 20,
  maxHammingDistance: Int = 1,
  cluster: Boolean = false,
  clusteringOpts: ClusteringOpts = ClusteringOpts()
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
  maxNbClusters: Int = 30
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
  referenceFiles: Seq[File] = Seq(),
  maxNbReferencePredictions: Option[Int] = None
)
