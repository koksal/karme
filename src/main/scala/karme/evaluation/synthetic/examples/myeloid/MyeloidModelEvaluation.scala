package karme.evaluation.synthetic.examples.myeloid

import karme.evaluation.PerturbationAnalysis
import karme.evaluation.synthetic.FixpointStates
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.Transitions.ConcreteBooleanState

object MyeloidModelEvaluation {

  def evaluateModelBehavior(
    labelToFun: Map[String, FunExpr]
  ): Map[String, Any] = {
    Map(
      "Missed WT FP" ->
        findMissedFixpoints(labelToFun).size,
      "Spurious WT FP" ->
        findUnexpectedFixpoints(labelToFun).size,
      "KO w/ inexact FP" ->
        nbDisagreeingKO(labelToFun)
    )
  }

  def findMissedFixpoints(
    labelToFun: Map[String, FunExpr]
  ): Set[ConcreteBooleanState] = {
    val simulationFixpoints = FixpointStates.findSimulationFixpoints(
      labelToFun, Set(MyeloidModel.makeInitialState()))
    val expectedFixpoints = MyeloidModel.stableStates()
    expectedFixpoints -- simulationFixpoints
  }

  def findUnexpectedFixpoints(
    labelToFun: Map[String, FunExpr]
  ): Set[ConcreteBooleanState] = {
    val simulationFixpoints = FixpointStates.findSimulationFixpoints(
      labelToFun, Set(MyeloidModel.makeInitialState()))
    val expectedFixpoints = MyeloidModel.stableStates()
    simulationFixpoints -- expectedFixpoints
  }

  def nbDisagreeingKO(labelToFun: Map[String, FunExpr]): Int = {
    var nbDisagreeing = 0

    for (ke <- MyeloidModel.knockoutExperiments()) {
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

      if (fixpointCellTypeIds != ke.observedOriginalAttractors) {
        nbDisagreeing += 1
      }

    }

    nbDisagreeing
  }

}
