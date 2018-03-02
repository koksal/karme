package karme.evaluation.synthetic

import karme.evaluation.synthetic.examples.myeloid.MyeloidModel
import karme.transformations.DistributionComparisonTest
import karme.ArgHandling
import karme.Reporter

import scala.util.Random

object Workflow {

  def main(args: Array[String]): Unit = {
    implicit val opts = ArgHandling.parseOptions(args)
    implicit val reporter = new Reporter(opts.reporterOpts)

    new SyntheticWorkflow(
      hiddenModel = MyeloidModel.makeTrimmedStateSpaceNetwork(),
      defaultInitialStates = Set(MyeloidModel.makeInitialState()),
      targetStates = MyeloidModel.stableStates,
      inputTransformerOpts = opts.inputTransformerOpts,
      synthOpts = opts.synthOpts,
      syntheticEvalOpts = opts.syntheticEvalOpts
    )(reporter).run()
  }

}
