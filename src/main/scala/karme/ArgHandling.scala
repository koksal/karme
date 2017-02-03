package karme

import java.io.File

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

      opt[File]("contexp") action { (v, o) =>
        o.copy(continuousExperimentFile = Some(v))
      } text "continuous experiment file in CSV format"

      opt[File]("discrexp") action { (v, o) =>
        o.copy(discretizedExperimentFile = Some(v))
      } text "discretized experiment file in CSV format"

      opt[File]("mle") action { (v, o) =>
        o.copy(mleExperimentFile = Some(v))
      } text "MLE experiment file in CSV format"

      opt[File]("clusters") action { (v, o) =>
        o.copy(clusterFile = Some(v))
      } text "cluster file in CSV format"

      opt[Seq[File]]("names") action { (vs, o) =>
        o.copy(namesFiles = vs)
      } text "names files to filter experiment with"

      opt[Seq[File]]("trajectory") action { (vs, o) =>
        o.copy(trajectoryFiles = vs)
      } text "trajectory files in CSV format"

      opt[String]("outfolder") action { (v, o) =>
        o.copy(outFolder = new File(v))
      } text "output folder"

      opt[Unit]("visualize") action { (_, o) =>
        o.copy(visualizationOptions = VisualizationOptions(true, true, true,
          true))
      } text "run all visualizations"

      opt[Unit]("graphs") action { (_, o) =>
        o.copy(visualizationOptions =
          o.visualizationOptions.copy(stateGraph = true))
      } text "visualize state graphs"

      opt[Unit]("discrete-analysis") action { (_, o) =>
        o.copy(discreteAnalysis = true)
      } text "run discrete data analysis"

      opt[Unit]("continuous-analysis") action { (_, o) =>
        o.copy(continuousAnalysis = true)
      } text "run continuous data analysis"

      opt[Int]("window-radius") action { (i, o) =>
        o.copy(analysisOptions = o.analysisOptions.copy(windowRadius = i))
      } text "window radius for binomial MLE pass"

      opt[Int]("hamming") action { (i, o) =>
        o.copy(analysisOptions = o.analysisOptions.copy(maxHammingDistance = i))
      } text "maximum hamming distance in state graph"

      opt[Int]("nbclusters") action { (i, o) =>
        o.copy(analysisOptions = o.analysisOptions.copy(nbClusters = Some(i)))
      } text "number of clusters to reduce experiment"

      help("help") text "print this help message"
    }

  }

}
