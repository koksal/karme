package karme.evaluation.synthetic

import karme.ArgHandling
import karme.Reporter

object Evaluation {

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)
    val reporter = new Reporter(opts.reporterOpts)

    new SyntheticWorkflow(opts, reporter).plotTimestamps()
  }

}
