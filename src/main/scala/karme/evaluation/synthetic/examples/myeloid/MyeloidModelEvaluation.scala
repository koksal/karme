package karme.evaluation.synthetic.examples.myeloid

import karme.evaluation.PerturbationAnalysis
import karme.evaluation.synthetic.{ClassificationEval, FixpointStates}
import karme.simulation.AsyncBooleanNetworkSimulation
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.Transitions.ConcreteBooleanState

object MyeloidModelEvaluation {

  def evaluateWildTypeFixpoints(
    modelToEvaluate: Map[String, FunExpr],
    referenceModel: Map[String, FunExpr]
  ): Map[String, Any] = {
    evaluateFixpoints(
      modelToEvaluate,
      referenceModel,
      Set(MyeloidModel.makeInitialState())
    )
  }

  def evaluateWildTypeReachability(
    modelToEvaluate: Map[String, FunExpr],
    referenceModel: Map[String, FunExpr]
  ): Map[String, Any] = {
    evaluateReachability(
      modelToEvaluate,
      referenceModel,
      Set(MyeloidModel.makeInitialState())
    )
  }

  def evaluateKnockoutFixpoints(
    modelToEvaluate: Map[String, FunExpr],
    referenceModel: Map[String, FunExpr]
  ): Seq[Map[String, Any]] = {
    evaluateKnockout(modelToEvaluate, referenceModel, evaluateFixpoints)
  }

  def evaluateKnockoutReachability(
    modelToEvaluate: Map[String, FunExpr],
    referenceModel: Map[String, FunExpr]
  ): Seq[Map[String, Any]] = {
    evaluateKnockout(modelToEvaluate, referenceModel, evaluateReachability)
  }

  private def evaluateKnockout(
    modelToEvaluate: Map[String, FunExpr],
    referenceModel: Map[String, FunExpr],
    evalFun:
      (Map[String, FunExpr], Map[String, FunExpr], Set[ConcreteBooleanState]) =>
      Map[String, Any]
  ): Seq[Map[String, Any]] = {
    for (ke <- MyeloidModel.knockoutExperiments()) yield {
      val perturbedModelToEvaluate =
        PerturbationAnalysis.knockoutVariable(modelToEvaluate, ke.knockoutVar)
      val perturbedReferenceModel =
        PerturbationAnalysis.knockoutVariable(referenceModel, ke.knockoutVar)

      val perturbedInitialState = MyeloidModel.makeInitialState().replaceValue(
        ke.knockoutVar, false)

      evalFun(
        perturbedModelToEvaluate,
        perturbedReferenceModel,
        Set(perturbedInitialState)
      )
    }
  }

  private def evaluateFixpoints(
    modelToEvaluate: Map[String, FunExpr],
    referenceModel: Map[String, FunExpr],
    initialStates: Set[ConcreteBooleanState]
  ): Map[String, Any] = {
    val simulationFixpoints = FixpointStates.findSimulationFixpoints(
      modelToEvaluate, initialStates)
    val expectedFixpoints = FixpointStates.findSimulationFixpoints(
      referenceModel, initialStates)

    ClassificationEval.evaluate(
      simulationFixpoints,
      expectedFixpoints,
      MyeloidModel.allStates -- expectedFixpoints
    )
  }

  private def evaluateReachability(
    modelToEvaluate: Map[String, FunExpr],
    referenceModel: Map[String, FunExpr],
    initialStates: Set[ConcreteBooleanState]
  ): Map[String, Any] = {
    val simulationStates = AsyncBooleanNetworkSimulation
      .simulateOneStep(modelToEvaluate, initialStates)
    val expectedStates = AsyncBooleanNetworkSimulation
      .simulateOneStep(referenceModel, initialStates)

    ClassificationEval.evaluate(
      simulationStates,
      expectedStates,
      MyeloidModel.allStates -- expectedStates
    )
  }

}
