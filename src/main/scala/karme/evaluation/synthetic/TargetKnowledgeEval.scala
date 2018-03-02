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

    for ((hiddenVars, partialTargets) <- partialTargetSets) {
      val subReporterName = s"hidden=${hiddenVars.mkString(",")}"
      val subReporter = mainReporter.subfolderReporter(subReporterName)

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
