package karme.evaluation.synthetic

import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.util.CollectionUtil

object InitialStatePerturbationEval {

  def compareInferredFixpointsForPerturbations(
    hiddenModel: Map[String, FunExpr],
    inferredModel: Map[String, FunExpr],
    initStates: Set[ConcreteBooleanState]
  ): Map[String, Any] = {
    hiddenModel.keySet.map { v =>
      val hiddenPerturbedFixpoints = fixpointsForPerturbedInitStates(
        hiddenModel, initStates, v
      )
      val inferredPerturbedFixopints = fixpointsForPerturbedInitStates(
        inferredModel, initStates, v
      )
      val jaccardSim = CollectionUtil.jaccardSimilarity(
        hiddenPerturbedFixpoints,
        inferredPerturbedFixopints
      )
      v -> jaccardSim
    }.toMap
  }

  def jaccardSimilarityForPerturbedInitStates(
    model: Map[String, FunExpr],
    initStates: Set[ConcreteBooleanState]
  ): Map[String, Any] = {
    val originalFixpoints = FixpointStates.findSimulationFixpoints(model,
      initStates)

    model.keySet.map { v =>
      val jaccardSim = CollectionUtil.jaccardSimilarity(
        originalFixpoints,
        fixpointsForPerturbedInitStates(model, initStates, v)
      )
      v -> jaccardSim
    }.toMap
  }

  private def fixpointsForPerturbedInitStates(
    model: Map[String, FunExpr],
    initStates: Set[ConcreteBooleanState],
    varToPerturb: String
  ): Set[ConcreteBooleanState] = {
    val perturbedStates = initStates.map(s => s.replaceValue(varToPerturb,
      !s.value(varToPerturb)))
    FixpointStates.findSimulationFixpoints(model, perturbedStates)
  }
}
