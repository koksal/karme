package karme.evaluation.synthetic

import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.util.CollectionUtil

object InitialStatePerturbationEval {

  def compareModelsForFixpointsFromPerturbedStates(
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
      v -> setComparisonSummary(
        hiddenPerturbedFixpoints,
        inferredPerturbedFixopints
      )
    }.toMap
  }

  def fixpointSimilarityInitialVsPerturbedState(
    model: Map[String, FunExpr],
    initStates: Set[ConcreteBooleanState]
  ): Map[String, Any] = {
    val originalFixpoints = FixpointStates.findSimulationFixpoints(model,
      initStates)

    model.keySet.map { v =>
      v -> setComparisonSummary(
        originalFixpoints,
        fixpointsForPerturbedInitStates(model, initStates, v)
      )
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

  private def setComparisonSummary[A](s1: Set[A], s2: Set[A]): String = {
    val nbCommon = s1.intersect(s2).size
    val onlyIn1 = (s1 -- s2).size
    val onlyIn2 = (s2 -- s1).size
    val jaccardSim = CollectionUtil.jaccardSimilarity(s1, s2)
    s"$nbCommon/$onlyIn1/$onlyIn2/${jaccardSim}"
  }
}
