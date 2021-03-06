package karme.evaluation.synthetic

import karme.synthesis.Transitions.ConcreteBooleanState
import karme.util.CollectionUtil

object PartialStateEnumeration {

  def makePartialStates(
    completeStates: Set[ConcreteBooleanState],
    nbVarsToHide: Int
  ): Set[(Set[String], Set[ConcreteBooleanState])] = {
    assert(completeStates.nonEmpty, "No states to make partial.")

    val names = completeStates.head.orderedKeys
    assert(completeStates.forall(s => s.orderedKeys == names),
      "States have different dimensions.")

    for (
      varsToHide <- CollectionUtil.combinations(names.toSet, nbVarsToHide)
    ) yield {
      val partialStates = completeStates map (s => removeVars(s, varsToHide))
      (varsToHide, partialStates)
    }
  }

  private def removeVars(
    state: ConcreteBooleanState, varsToHide: Set[String]
  ): ConcreteBooleanState = {
    state.copy(mapping = state.mapping -- varsToHide)
  }

}
