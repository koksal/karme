package karme.evaluation.synthetic.examples.myeloid

import karme.evaluation.PerturbationAnalysis
import karme.evaluation.synthetic.ClassificationEval
import karme.evaluation.synthetic.FixpointStates
import karme.evaluation.synthetic.stategen.ExhaustiveStateEnumeration
import karme.synthesis.FunctionTrees.FunExpr

object MyeloidModelEvaluation {

  def evaluateWildTypeBehavior(
    labelToFun: Map[String, FunExpr]
  ): Map[String, Any] = {
    val simulationFixpoints = FixpointStates.findSimulationFixpoints(
      labelToFun, Set(MyeloidModel.makeInitialState()))
    val expectedFixpoints = MyeloidModel.stableStates()
    val allStates = new ExhaustiveStateEnumeration(
      MyeloidModel.makeInitialState().orderedKeys).enumerateAllStates()

    ClassificationEval.evaluate(
      simulationFixpoints,
      expectedFixpoints,
      allStates -- expectedFixpoints
    )
  }

  def evaluateKnockoutBehavior(
    labelToFun: Map[String, FunExpr]
  ): Seq[Map[String, Any]] = {
    for (ke <- MyeloidModel.knockoutExperiments()) yield {
      val perturbedFuns = PerturbationAnalysis.knockoutVariable(labelToFun,
        ke.knockoutVar)

      val perturbedInitialState = MyeloidModel.makeInitialState().replaceValue(
        ke.knockoutVar, false)

      val simulationFixpoints = FixpointStates.findSimulationFixpoints(
        perturbedFuns, Set(perturbedInitialState))

      val simFixpointCellTypes = MyeloidModel.namedStableStates() filter {
        case (_, state) => simulationFixpoints.contains(state)
      }

      val fixpointCellTypeIds = simFixpointCellTypes.keySet

      ClassificationEval.evaluate(
        fixpointCellTypeIds,
        ke.observedOriginalAttractors,
        MyeloidModel.allCellTypeIDs -- ke.observedOriginalAttractors
      )
    }
  }

}
