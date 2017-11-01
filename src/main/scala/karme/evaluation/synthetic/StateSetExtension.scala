package karme.evaluation.synthetic

import karme.evaluation.synthetic.stategen.ExhaustiveStateEnumeration
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.util.CollectionUtil

object StateSetExtension {

  def randomStateSet(
    variables: Seq[String],
    stateInclusionRatio: Double
  ): Set[ConcreteBooleanState] = {
    val allStates = new ExhaustiveStateEnumeration(variables)
      .enumerateAllStates()

    val nbElems = (allStates.size * stateInclusionRatio).toInt

    CollectionUtil.randomElements(allStates, nbElems).toSet
  }

}
