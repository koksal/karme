package karme.evaluation.synthetic

import karme.ArgHandling
import karme.Reporter
import karme.evaluation.synthetic.examples.CAVModel
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.Transitions.ConcreteBooleanState

object Evaluation {

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)
    implicit val reporter = new Reporter(opts.reporterOpts)

    run(
      hiddenModel = CAVModel.makePlosNetwork(),
      initialStates = Set(CAVModel.makeInitialState()),
      initialStateExtensionRatio = 0.0,
      nodeDeletionRatio = 0.0,
      reconstructGraph = false
    )
  }

  def run(
    hiddenModel: Map[String, FunExpr],
    initialStates: Set[ConcreteBooleanState],
    initialStateExtensionRatio: Double,
    nodeDeletionRatio: Double,
    reconstructGraph: Boolean
  )(implicit reporter: Reporter): Unit = {
    // modify initial states per extension ratio
    // run simulation
    // remove nodes per deletion ratio
    // reconstruct graph per flag
    // perform synthesis
    // evaluate behavior, state space, function similarity
    // interleave logging of results
  }

}
