package karme

import java.io.File

import karme.discretization.Ckmeans
import karme.discretization.Mclust
import scopt.OptionParser

/**
  * Created by ask on 10/17/16.
  */
object ArgHandling {
  def parseOptions(args: Array[String]) = {
    val opts = new Options()
    ArgHandling.parser.parse(args, opts) getOrElse {
      sys.error("Bad arguments.")
    }
  }

  private def parser = {
    new OptionParser[Options]("karme") {
      head("karme", "1.0")

      // Input files:

      opt[File]("contexp") action { (v, o) =>
        o.copy(continuousExperimentFile = Some(v))
      } text "continuous experiment file in CSV format"

      opt[File]("discrexp") action { (v, o) =>
        o.copy(discretizedExperimentFile = Some(v))
      } text "discretized experiment file in CSV format"

      opt[File]("mle") action { (v, o) =>
        o.copy(mleExperimentFile = Some(v))
      } text "MLE experiment file in CSV format"

      opt[File]("cell-clusters") action { (v, o) =>
        o.copy(clusterFile = Some(v))
      } text "cell clustering file in CSV format"

      opt[Seq[File]]("names") action { (vs, o) =>
        o.copy(namesFiles = vs)
      } text "names files to filter experiment with"

      opt[Seq[File]]("annotations") action { (vs, o) =>
        o.copy(annotationsFiles = vs)
      } text "files with variables to annotate clusters with."

      opt[Seq[File]]("trajectory") action { (vs, o) =>
        o.copy(trajectoryFiles = vs)
      } text "trajectory files in CSV format"

      // Output:

      opt[String]("outfolder") action { (v, o) =>
        o.copy(outFolder = new File(v))
      } text "output folder"

      // Pipeline options:

      opt[Unit]("elbow") action { (_, o) =>
        o.copy(runElbow = true)
      } text "run within-cluster sum of squares and plot results"

      opt[Unit]("synthesis") action { (_, o) =>
        o.copy(runSynthesis = true)
      } text "run function synthesis"

      opt[Unit]("simulation") action { (_, o) =>
        o.copy(runSimulation = true)
      } text "run function simulation"

      // Analysis options:

      opt[Double]("pseudolog-factor") action { (d, o) =>
        o.copy(analysisOptions = o.analysisOptions.copy(pseudoLogFactor =
          Some(d)))
      } text "pseudolog factor for transforming data"

      opt[String]("first-discretization") action { (v, o) =>
        val method = v match {
          case "kmeans" => Ckmeans
          case "mclust" => Mclust
          case _ => sys.error(s"Unrecognized discretization method: $v")
        }
        o.copy(analysisOptions = o.analysisOptions.copy(
          firstDiscretizationMethod = method))
      } text "first discretization method"

      opt[Double]("activity-ratio") action { (d, o) =>
        o.copy(analysisOptions =
          o.analysisOptions.copy(cellActivityThreshold = d))
      } text "ratio of cells in which a gene must be active"

      opt[Unit]("force-annotations") action { (_, o) =>
        o.copy(analysisOptions = o.analysisOptions.copy(forceAnnotations =
          true))
      } text "force all annotation variables to be included in analysis"

      opt[Double]("uncertainty") action { (d, o) =>
        o.copy(analysisOptions = o.analysisOptions.copy(uncertaintyMargin = d))
      } text "maximum distance from middle MLE value for uncertain values"

      opt[Int]("smoothing-radius") action { (i, o) =>
        o.copy(analysisOptions = o.analysisOptions.copy(smoothingRadius = i))
      } text "cell vicinity radius for binomial MLE pass"

      opt[Int]("hamming") action { (i, o) =>
        o.copy(analysisOptions = o.analysisOptions.copy(maxHammingDistance = i))
      } text "maximum hamming distance in state graph"

      opt[Int]("nbclusters") action { (i, o) =>
        o.copy(analysisOptions = o.analysisOptions.copy(nbClusters = Some(i)))
      } text "number of clusters to reduce experiment"

      // Visualization options:

      opt[Unit]("visualize") action { (_, o) =>
        o.copy(visualizationOptions = VisualizationOptions(true, true, true,
          true))
      } text "run all visualizations"

      opt[Unit]("graphs") action { (_, o) =>
        o.copy(visualizationOptions =
          o.visualizationOptions.copy(stateGraphs = true))
      } text "visualize state graphs"

      opt[Unit]("curves") action { (_, o) =>
        o.copy(visualizationOptions =
          o.visualizationOptions.copy(curves = true))
      } text "visualize trajectory curves"

      help("help") text "print this help message"
    }

  }

}
