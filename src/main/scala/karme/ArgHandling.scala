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

      opt[String]("contexp") action { (v, o) =>
        o.copy(continuousExperimentFile = Some(new File(v)))
      } text "continuous experiment file in CSV format"

      opt[String]("discrexp") action { (v, o) =>
        o.copy(discreteExperimentFile = Some(new File(v)))
      } text "discrete experiment file in CSV format"

      opt[String]("clusters") action { (v, o) =>
        o.copy(clusterFile = Some(new File(v)))
      } text "cluster file in CSV format"

      opt[String]("names") action { (v, o) =>
        o.copy(namesFile = Some(new File(v)))
      } text "file with names to project experiment to"

      opt[String]("outfolder") action { (v, o) =>
        o.copy(outFolder = new File(v))
      } text "output folder"

      opt[Unit]("discretize") action { (_, o) =>
        o.copy(discretize = true)
      } text "discretize data"

      opt[Unit]("visualize") action { (_, o) =>
        o.copy(visualize = true)
      } text "visualize discretization data"

      opt[Unit]("discrete-analysis") action { (_, o) =>
        o.copy(discreteAnalysis = true)
      } text "run discrete data analysis"

      opt[Unit]("continuous-analysis") action { (_, o) =>
        o.copy(continuousAnalysis = true)
      } text "run continuous data analysis"

      help("help") text "print this help message"
    }

  }

}
