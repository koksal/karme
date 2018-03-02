package karme.evaluation.synthetic

import karme.ArgHandling
import karme.Reporter
import karme.evaluation.synthetic.examples.myeloid.MyeloidModel

object TargetKnowledgeEval {

  def main(args: Array[String]): Unit = {
    implicit val opts = ArgHandling.parseOptions(args)
    implicit val mainReporter = new Reporter(opts.reporterOpts)

    // enumerate partial stable states
    val partialTargetSets = PartialStateEnumeration.makePartialStates(
      MyeloidModel.stableStates, 1)

    for ((partialTargets, i) <- partialTargetSets.zipWithIndex) {
      val subReporter = mainReporter.subfolderReporter(s"partial-targets-$i")

      new SyntheticWorkflow(
        hiddenModel = MyeloidModel.makeTrimmedStateSpaceNetwork(),
        defaultInitialStates = Set(MyeloidModel.makeInitialState()),
        targetStates = partialTargets,
        inputTransformerOpts = opts.inputTransformerOpts,
        synthOpts = opts.synthOpts,
        syntheticEvalOpts = opts.syntheticEvalOpts
      )(subReporter).run()
    }

  }

}
