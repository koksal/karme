package karme

import java.io.File

import karme.evaluation.PredictionTypes.{FunIOPairsPrediction, PrecedencePairsPrediction}
import karme.transformations.discretization.{Ckmeans, Mclust}
import scopt.OptionParser

object ArgHandling {
  def parseOptions(args: Array[String]) = {
    val opts = new Opts()
    ArgHandling.parser.parse(args, opts) getOrElse {
      sys.error("Bad arguments.")
    }
  }

  private def parser = {
    new OptionParser[Opts]("karme") {
      head("karme", "1.0")

      // Reporter:

      opt[String]("outfolder") action { (v, o) =>
        o.copy(reporterOpts = o.reporterOpts.copy(outFolder = new File(v)))
      } text "output folder"

      opt[Unit]("verbose") action { (_, o) =>
        o.copy(reporterOpts = o.reporterOpts.copy(verbose = true))
      } text "verbose output"

      // Input files:

      opt[File]("continuous-experiment") action { (v, o) =>
        o.copy(inputFileOpts =
          o.inputFileOpts.copy(continuousExperimentFile = Some(v)))
      } text "continuous experiment file in CSV format"

      opt[File]("discretized-experiment") action { (v, o) =>
        o.copy(inputFileOpts =
          o.inputFileOpts.copy(discretizedExperimentFile = Some(v)))
      } text "discretized experiment file in CSV format"

      opt[File]("smoothed-experiment") action { (v, o) =>
        o.copy(inputFileOpts =
          o.inputFileOpts.copy(smoothedExperimentFile = Some(v)))
      } text "Smoothed experiment file in CSV format"

      opt[Seq[File]]("names") action { (vs, o) =>
        o.copy(inputFileOpts = o.inputFileOpts.copy(namesFiles = vs))
      } text "names files to filter experiment with (uses the union of files)"

      opt[Seq[File]]("trajectories") action { (vs, o) =>
        o.copy(inputFileOpts = o.inputFileOpts.copy(trajectoryFiles = vs))
      } text "trajectory files in CSV format"

      opt[Seq[File]]("knockdown-experiments") action { (fs, o)  =>
        o.copy(inputFileOpts =
          o.inputFileOpts.copy(knockdownExperimentFiles = fs))
      } text "knockdown experiment files"

      // Annotation files:

      opt[File]("cell-clusters") action { (v, o) =>
        o.copy(annotationOpts =
          o.annotationOpts.copy(cellClusteringFile = Some(v)))
      } text "cell clustering file in CSV format"

      opt[Seq[File]]("annotations") action { (vs, o) =>
        o.copy(annotationOpts =
          o.annotationOpts.copy(annotationsFiles = vs))
      } text "files with variables to annotate clusters with."

      // Pipeline options:

      opt[Unit]("synthesis") action { (_, o) =>
        o.copy(runSynthesis = true)
      } text "run function synthesis"

      // Analysis options:

      opt[Double]("pseudolog-factor") action { (d, o) =>
        o.copy(inputTransformerOpts =
          o.inputTransformerOpts.copy(pseudoLogFactor = Some(d)))
      } text "pseudolog factor for transforming data"

      opt[String]("boolean-normalization") action { (v, o) =>
        val method = v match {
          case "kmeans" => Ckmeans
          case "mclust" => Mclust
          case _ => sys.error(s"Unrecognized discretization method: $v")
        }
        o.copy(inputTransformerOpts = o.inputTransformerOpts.copy(
          booleanNormalizationMethod = method))
      } text "discretization method for Boolean normalization"

      opt[Double]("cell-activity-threshold") action { (d, o) =>
        o.copy(inputTransformerOpts =
          o.inputTransformerOpts.copy(minDifferentialThreshold = d))
      } text "ratio of cells in which a gene must be active"

      opt[Double]("uncertainty-threshold") action { (d, o) =>
        o.copy(inputTransformerOpts =
          o.inputTransformerOpts.copy(uncertaintyThreshold = d))
      } text "uncertainty threshold for considering a discretized value as " +
        "uncertain"

      opt[Int]("smoothing-radius") action { (i, o) =>
        o.copy(inputTransformerOpts =
          o.inputTransformerOpts.copy(smoothingRadius = i))
      } text "cell vicinity radius for binomial MLE pass"

      opt[Int]("max-hamming") action { (i, o) =>
        o.copy(inputTransformerOpts =
          o.inputTransformerOpts.copy(maxHammingDistance = i))
      } text "maximum hamming distance in state graph"

      opt[Unit]("cluster") action { (_, o) =>
        o.copy(inputTransformerOpts =
          o.inputTransformerOpts.copy(clusteringOpts =
            o.inputTransformerOpts.clusteringOpts.copy(cluster = true)))
      } text "perform gene clustering"

      opt[Int]("min-clusters") action { (i, o) =>
        o.copy(inputTransformerOpts =
          o.inputTransformerOpts.copy(clusteringOpts =
            o.inputTransformerOpts.clusteringOpts.copy(minNbClusters = i)))
      } text "min number of gene clusters"

      opt[Int]("max-clusters") action { (i, o) =>
        o.copy(inputTransformerOpts =
          o.inputTransformerOpts.copy(clusteringOpts =
            o.inputTransformerOpts.clusteringOpts.copy(maxNbClusters = i)))
      } text "max number of gene clusters"

      opt[String]("clustering-index") action { (s, o) =>
        o.copy(inputTransformerOpts =
          o.inputTransformerOpts.copy(clusteringOpts =
            o.inputTransformerOpts.clusteringOpts.copy(clusteringIndex = s)))
      } text "index for choosing the number of gene clusters"

      opt[String]("clustering-method") action { (s, o) =>
        o.copy(inputTransformerOpts =
          o.inputTransformerOpts.copy(clusteringOpts =
            o.inputTransformerOpts.clusteringOpts.copy(clusteringMethod = s)))
      } text "method for clustering genes"

      opt[String]("clustering-distance") action { (s, o) =>
        o.copy(inputTransformerOpts =
          o.inputTransformerOpts.copy(clusteringOpts =
            o.inputTransformerOpts.clusteringOpts.copy(clusteringDistance = s)))
      } text "clustering distance measure"

      opt[String]("distribution-comparison") action { (s, o) =>
        o.copy(inputTransformerOpts = o.inputTransformerOpts.copy(
          distributionComparisonMethod = s))
      } text "Distribution comparison method: ks or ranksum"

      opt[Double]("distribution-comparison-p-value") action { (v, o) =>
        o.copy(inputTransformerOpts = o.inputTransformerOpts.copy(
          distributionComparisonPValue = v))
      } text "Distribution comparison p-value"

      opt[Unit]("refine-clusters") action { (_, o) =>
        o.copy(inputTransformerOpts =
          o.inputTransformerOpts.copy(clusteringOpts =
            o.inputTransformerOpts.clusteringOpts.copy(refineClusters = true)))
      } text "refine clusters using rank-sum method for each edge"

      opt[Double]("cluster-refinement-p-value") action { (d, o) =>
        o.copy(inputTransformerOpts =
          o.inputTransformerOpts.copy(clusteringOpts =
            o.inputTransformerOpts.clusteringOpts.copy(
              clusterRefinementPValue = d)))
      } text "p-value threshold for edge-level clustering refinement"

      opt[Unit]("plot-original-data") action { (_, o) =>
        o.copy(inputTransformerOpts =
          o.inputTransformerOpts.copy(plotOriginalData = true))
      } text "plot histograms for each gene in the input data"

      opt[Unit]("plot-transformed-data") action { (_, o) =>
        o.copy(inputTransformerOpts =
          o.inputTransformerOpts.copy(plotTransformedData = true))
      } text "plot histograms for each gene in transformed data"

      opt[Unit]("plot-binarized-data") action { (_, o) =>
        o.copy(inputTransformerOpts =
          o.inputTransformerOpts.copy(plotBinarizedData = true))
      } text "plot histograms for each gene in binarized data"

      opt[Unit]("plot-binarized-curves") action { (_, o) =>
        o.copy(inputTransformerOpts =
          o.inputTransformerOpts.copy(plotBinarizedGeneCurves = true))
      } text "plot binarized gene curves"

      opt[Unit]("plot-smoothed-curves") action { (_, o) =>
        o.copy(inputTransformerOpts =
          o.inputTransformerOpts.copy(plotSmoothedGeneCurves = true))
      } text "plot smoothed gene curves"

      opt[Unit]("plot-cluster-curves") action { (_, o) =>
        o.copy(inputTransformerOpts =
          o.inputTransformerOpts.copy(plotClusterCurves = true))
      } text "plot gene cluster curves"

      opt[Unit]("plot-three-valued-data") action { (_, o) =>
        o.copy(inputTransformerOpts =
          o.inputTransformerOpts.copy(plotThreeValuedData = true))
      } text "plot histograms with ternary labels"

      // Synthesis options:

      opt[Int]("max-expr-depth") action { (i, o) =>
        o.copy(synthOpts = o.synthOpts.copy(
          maxExpressionDepth = i))
      } text "maximum expression depth for inferred functions"

      opt[Int]("max-nb-models") action { (i, o) =>
        o.copy(synthOpts = o.synthOpts.copy(
          maxNbModels = Some(i)))
      } text "maximum number of equivalent functions enumerated"

      // Evaluation options:

      opt[File]("perturbation-targets") action { (f, o) =>
        o.copy(evalOpts = o.evalOpts.copy(perturbationTargetsFile = Some(f)))
      } text "List of target genes to observe in perturbation analysis."

      opt[File]("expected-drivers") action { (f, o) =>
        o.copy(evalOpts = o.evalOpts.copy(expectedDriversFile = Some(f)))
      } text "List of genes expected to affect perturbation targets."

      opt[Seq[File]]("evaluation-libraries") action { (fs, o) =>
        o.copy(evalOpts = o.evalOpts.copy(referenceFiles = fs))
      } text "Prediction libraries to evaluate against"

      opt[File]("predictions") action { (f, o) =>
        o.copy(evalOpts = o.evalOpts.copy(predictionPairsFile = Some(f)))
      } text "File with predictions to evaluate"

      opt[File]("run-collection") action { (f, o) =>
        o.copy(evalOpts = o.evalOpts.copy(runCollectionFolder = Some(f)))
      } text "Folder containing run folders to evaluate."

      opt[String]("prediction-type") action { (s, o) =>
        val predictionType = s match {
          case "fun-io-pairs" => FunIOPairsPrediction
          case "precedence-pairs" => PrecedencePairsPrediction
        }
        o.copy(evalOpts = o.evalOpts.copy(predictionType = predictionType))
      } text "Evaluation type (function IO pairs or precedence pairs"

      opt[Int]("max-precedence-distance") action { (i, o) =>
        o.copy(evalOpts = o.evalOpts.copy(maxPrecedenceDistance = Some(i)))
      } text "Maximum allowed precedence distance for predictions."

      opt[Unit]("normalize-scores") action { (_, o) =>
        o.copy(evalOpts = o.evalOpts.copy(normalizeScores = true))
      } text "Normalize prediction scores"

      opt[Unit]("randomize-eval") action { (_, o) =>
        o.copy(evalOpts = o.evalOpts.copy(randomize = true))
      } text "Randomize predictions before evaluation"

      // Synthetic evaluation options:

      opt[Int]("random-seed") action { (i, o) =>
        o.copy(syntheticEvalOpts = o.syntheticEvalOpts.copy(
          randomSeed = i))
      }

      opt[Double]("cell-trajectory-noise-sigma") action { (v, o) =>
        o.copy(syntheticEvalOpts = o.syntheticEvalOpts.copy(
          cellTrajectoryNoiseSigma = v))
      }

      opt[Double]("type-i-error-ratio") action { (v, o) =>
        o.copy(syntheticEvalOpts = o.syntheticEvalOpts.copy(
          typeIErrorRatio = v))
      }

      opt[Double]("type-ii-error-ratio") action { (v, o) =>
        o.copy(syntheticEvalOpts = o.syntheticEvalOpts.copy(
          typeIIErrorRatio = v))
      }

      opt[Double]("randomized-initial-states-ratio") action { (v, o) =>
        o.copy(syntheticEvalOpts = o.syntheticEvalOpts.copy(
          randomizedInitialStateInclusionRatio = Some(v)))
      }

      help("help") text "print this help message"
    }

  }

}
