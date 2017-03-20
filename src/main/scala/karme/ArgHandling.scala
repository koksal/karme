package karme

import java.io.File

import karme.transformations.discretization.Ckmeans
import karme.transformations.discretization.Mclust
import scopt.OptionParser

/**
  * Created by ask on 10/17/16.
  */
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

      // Output:

      opt[String]("outfolder") action { (v, o) =>
        o.copy(outFolder = new File(v))
      } text "output folder"

      // Input files:

      opt[File]("continuous-experiment") action { (v, o) =>
        o.copy(
          synthInputBuilderOpts = o.synthInputBuilderOpts.copy(
            inputFileOpts = o.synthInputBuilderOpts.inputFileOpts.copy(
              continuousExperimentFile = Some(v))))
      } text "continuous experiment file in CSV format"

      opt[File]("discretized-experiment") action { (v, o) =>
        o.copy(
          synthInputBuilderOpts = o.synthInputBuilderOpts.copy(
            inputFileOpts = o.synthInputBuilderOpts.inputFileOpts.copy(
              discretizedExperimentFile = Some(v))))
      } text "discretized experiment file in CSV format"

      opt[File]("smoothed-experiment") action { (v, o) =>
        o.copy(
          synthInputBuilderOpts = o.synthInputBuilderOpts.copy(
            inputFileOpts = o.synthInputBuilderOpts.inputFileOpts.copy(
              smoothedExperimentFile = Some(v))))
      } text "Smoothed experiment file in CSV format"

      opt[Seq[File]]("names") action { (vs, o) =>
        o.copy(
          synthInputBuilderOpts = o.synthInputBuilderOpts.copy(
            inputFileOpts = o.synthInputBuilderOpts.inputFileOpts.copy(
              namesFiles = vs)))
      } text "names files to filter experiment with"

      opt[Seq[File]]("trajectories") action { (vs, o) =>
        o.copy(
          synthInputBuilderOpts = o.synthInputBuilderOpts.copy(
            inputFileOpts = o.synthInputBuilderOpts.inputFileOpts.copy(
              trajectoryFiles = vs)))
      } text "trajectory files in CSV format"

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
        o.copy(synthInputBuilderOpts =
          o.synthInputBuilderOpts.copy(pseudoLogFactor = Some(d)))
      } text "pseudolog factor for transforming data"

      opt[String]("boolean-normalization") action { (v, o) =>
        val method = v match {
          case "kmeans" => Ckmeans
          case "mclust" => Mclust
          case _ => sys.error(s"Unrecognized discretization method: $v")
        }
        o.copy(synthInputBuilderOpts = o.synthInputBuilderOpts.copy(
          booleanNormalizationMethod = method))
      } text "discretization method for Boolean normalization"

      opt[Double]("cell-activity-threshold") action { (d, o) =>
        o.copy(synthInputBuilderOpts =
          o.synthInputBuilderOpts.copy(cellActivityThreshold = d))
      } text "ratio of cells in which a gene must be active"

      opt[Double]("uncertainty-threshold") action { (d, o) =>
        o.copy(synthInputBuilderOpts =
          o.synthInputBuilderOpts.copy(uncertaintyThreshold = d))
      } text "uncertainty threshold for considering a discretized value as " +
        "uncertain"

      opt[Int]("smoothing-radius") action { (i, o) =>
        o.copy(synthInputBuilderOpts =
          o.synthInputBuilderOpts.copy(smoothingRadius = i))
      } text "cell vicinity radius for binomial MLE pass"

      opt[Int]("max-hamming") action { (i, o) =>
        o.copy(synthInputBuilderOpts =
          o.synthInputBuilderOpts.copy(maxHammingDistance = i))
      } text "maximum hamming distance in state graph"

      opt[Unit]("cluster") action { (_, o) =>
        o.copy(synthInputBuilderOpts =
          o.synthInputBuilderOpts.copy(cluster = true))
      }

      opt[Int]("min-clusters") action { (i, o) =>
        o.copy(synthInputBuilderOpts =
          o.synthInputBuilderOpts.copy(clusteringOpts =
            o.synthInputBuilderOpts.clusteringOpts.copy(minNbClusters = i)))
      } text "min number of gene clusters"

      opt[Int]("max-clusters") action { (i, o) =>
        o.copy(synthInputBuilderOpts =
          o.synthInputBuilderOpts.copy(clusteringOpts =
            o.synthInputBuilderOpts.clusteringOpts.copy(maxNbClusters = i)))
      } text "max number of gene clusters"

      // Synthesis options:

      opt[Int]("max-expr-depth") action { (i, o) =>
        o.copy(synthOpts = o.synthOpts.copy(
          maxExpressionDepth = i))
      } text "maximum expression depth for inferred functions"

      opt[Int]("max-nb-models") action { (i, o) =>
        o.copy(synthOpts = o.synthOpts.copy(
          maxNbModels = Some(i)))
      } text "maximum number of equivalent functions enumerated"

      help("help") text "print this help message"
    }

  }

}
