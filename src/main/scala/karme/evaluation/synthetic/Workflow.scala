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
      random = new Random(opts.syntheticEvalOpts.randomSeed),
      cellTrajectoryNoiseSigma =
        opts.syntheticEvalOpts.cellTrajectoryNoiseSigma,
      typeIErrorRatio =
        opts.syntheticEvalOpts.typeIErrorRatio,
      typeIIErrorRatio =
        opts.syntheticEvalOpts.typeIIErrorRatio,
      randomizedInitialStateInclusionRatio =
        opts.syntheticEvalOpts.randomizedInitialStateInclusionRatio,
      distributionComparisonTest = DistributionComparisonTest.fromOptions(
        opts.inputTransformerOpts.distributionComparisonMethod),
      distCompPValueThreshold =
        opts.inputTransformerOpts.distributionComparisonPValue
    )(reporter, opts).run()
  }

}
