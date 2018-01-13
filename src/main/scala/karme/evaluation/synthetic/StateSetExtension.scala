package karme.evaluation.synthetic

import karme.evaluation.synthetic.stategen.ExhaustiveStateEnumeration
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.util.CollectionUtil

import scala.util.Random

object StateSetExtension {

  def randomStateSet(
    variables: Seq[String],
    stateInclusionRatio: Double,
    random: Random
  ): Set[ConcreteBooleanState] = {
    val allStates = new ExhaustiveStateEnumeration(variables)
      .enumerateAllStates()

    val nbElems = (allStates.size * stateInclusionRatio).toInt

    CollectionUtil.randomElements(random)(allStates, nbElems).toSet
  }

}
