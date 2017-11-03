package karme.evaluation.synthetic

import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.Transitions.ConcreteBooleanState

object InitialStatePerturbationEval {

  def compareFixpointsForInitStatePerturbations(
    hiddenModel: Map[String, FunExpr],
    inferredModel: Map[String, FunExpr],
    initStates: Set[ConcreteBooleanState]
  ): Map[String, Any] = {
    // per state and per variable difference between fixpoints
    ???
  }
}
