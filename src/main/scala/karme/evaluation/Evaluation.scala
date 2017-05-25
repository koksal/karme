package karme.evaluation

import karme.ArgHandling
import karme.Reporter
import karme.util.FileUtil

object Evaluation {

  type ScoredPrediction = ((String, String), Int)

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)
    val reporter = new Reporter(opts.reporterOpts)

    val evalCtx = EvaluationContext.fromOptions(opts.evalOpts)

    opts.evalOpts.runCollectionFolder match {
      case Some(f) => {
        val evaluator = new PairEvaluator(FileUtil.listFiles(f),
          evalCtx.references, opts.evalOpts, reporter)

        evaluator.evaluateFunctionIOPairs()
      }
      case None => {
        reporter.log("No run collection to evaluate.")
      }
    }
  }

}
