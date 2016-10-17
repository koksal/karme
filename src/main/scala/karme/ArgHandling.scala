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
        o.copy(continuousExperimentFile = new File(v))
      } text "continuous experiment file in CSV format"

      opt[String]("discrexp") action { (v, o) =>
        o.copy(discreteExperimentFile = new File(v))
      } text "discrete experiment file in CSV format"

      help("help") text "print this help message"
    }

  }

}
