package karme.evaluation.synthetic.examples.myeloid

import karme.evaluation.PerturbationAnalysis
import karme.evaluation.synthetic.{ClassificationEval, FixpointStates}
import karme.evaluation.synthetic.stategen.ExhaustiveStateEnumeration
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.Transitions.ConcreteBooleanState

object MyeloidModelEvaluation {

  def evaluateWildTypeBehavior(
    modelToEvaluate: Map[String, FunExpr],
    referenceModel: Map[String, FunExpr]
  ): Map[String, Any] = {
    evaluateBehavior(
      modelToEvaluate,
      referenceModel,
      Set(MyeloidModel.makeInitialState())
    )
  }

  def evaluateKnockoutBehavior(
    modelToEvaluate: Map[String, FunExpr],
    referenceModel: Map[String, FunExpr]
  ): Seq[Map[String, Any]] = {
    for (ke <- MyeloidModel.knockoutExperiments()) yield {
      val perturbedModelToEvaluate =
        PerturbationAnalysis.knockoutVariable(modelToEvaluate, ke.knockoutVar)
      val perturbedReferenceModel =
        PerturbationAnalysis.knockoutVariable(referenceModel, ke.knockoutVar)

      val perturbedInitialState = MyeloidModel.makeInitialState().replaceValue(
        ke.knockoutVar, false)

      evaluateBehavior(
        perturbedModelToEvaluate,
        perturbedReferenceModel,
        Set(perturbedInitialState)
      )
    }
  }

  private def evaluateBehavior(
    modelToEvaluate: Map[String, FunExpr],
    referenceModel: Map[String, FunExpr],
    initialStates: Set[ConcreteBooleanState]
  ): Map[String, Any] = {
    val simulationFixpoints = FixpointStates.findSimulationFixpoints(
      modelToEvaluate, Set(MyeloidModel.makeInitialState()))
    val expectedFixpoints = FixpointStates.findSimulationFixpoints(
      referenceModel, Set(MyeloidModel.makeInitialState()))
    val allStates = new ExhaustiveStateEnumeration(
      MyeloidModel.makeInitialState().orderedKeys).enumerateAllStates()

    ClassificationEval.evaluate(
      simulationFixpoints,
      expectedFixpoints,
      allStates -- expectedFixpoints
    )
  }

}
