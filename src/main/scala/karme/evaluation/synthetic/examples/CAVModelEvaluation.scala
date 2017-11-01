package karme.evaluation.synthetic.examples

import karme.evaluation.PerturbationAnalysis
import karme.evaluation.synthetic.SyntheticWorkflow
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.Transitions.ConcreteBooleanState

object CAVModelEvaluation {

  def evaluateModelBehavior(
    labelToFun: Map[String, FunExpr]
  ): Map[String, Any] = {
    Map(
      "Missed wild-type fixpoints " ->
        findMissedFixpoints(labelToFun).size,
      "Unexpected wild-type fixpoints" ->
        findUnexpectedFixpoints(labelToFun).size,
      "Perturbations disagreeing about expected cell-types" ->
        nbDisagreeingPerturbations(labelToFun)
    )
  }

  def findMissedFixpoints(
    labelToFun: Map[String, FunExpr]
  ): Set[ConcreteBooleanState] = {
    val simulationFixpoints = SyntheticWorkflow.findSimulationFixpoints(
      labelToFun, Set(CAVModel.makeInitialState()))
    val expectedFixpoints = CAVModel.myeloidStableStates().values.toSet
    expectedFixpoints -- simulationFixpoints
  }

  def findUnexpectedFixpoints(
    labelToFun: Map[String, FunExpr]
  ): Set[ConcreteBooleanState] = {
    val simulationFixpoints = SyntheticWorkflow.findSimulationFixpoints(
      labelToFun, Set(CAVModel.makeInitialState()))
    val expectedFixpoints = CAVModel.myeloidStableStates().values.toSet
    simulationFixpoints -- expectedFixpoints
  }

  def nbDisagreeingPerturbations(labelToFun: Map[String, FunExpr]): Int = {
    var nbDisagreeing = 0

    for (ke <- CAVModel.knockoutExperiments()) {
      val perturbedFuns = PerturbationAnalysis.knockoutVariable(labelToFun,
        ke.knockoutVar)

      val perturbedInitialState = CAVModel.makeInitialState().replaceValue(
        ke.knockoutVar, false)

      val simulationFixpoints = SyntheticWorkflow.findSimulationFixpoints(
        perturbedFuns, Set(perturbedInitialState))

      val simFixpointCellTypes = CAVModel.myeloidStableStates() filter {
        case (id, state) => simulationFixpoints.contains(state)
      }

      val fixpointCellTypeIds = simFixpointCellTypes.keySet

      if (fixpointCellTypeIds != ke.observedOriginalAttractors) {
        nbDisagreeing += 1
      }

    }

    nbDisagreeing
  }


}
