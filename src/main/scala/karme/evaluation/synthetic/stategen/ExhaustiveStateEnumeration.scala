package karme.evaluation.synthetic.stategen
import karme.synthesis.Transitions.{ConcreteBooleanState, GenericState}
import karme.util.MathUtil

class ExhaustiveStateEnumeration(
  variables: Seq[String]
) extends StateEnumeration {

  def enumerateInitialStates(): Seq[Set[ConcreteBooleanState]] = {
    enumerateAllStates().toList map (s => Set(s))
  }

  def enumerateAllStates(): Set[ConcreteBooleanState] = {
    val setsForProduct = variables map {
      v => Set(true, false)
    }

    val product = MathUtil.cartesianProduct(setsForProduct.toList)

    product map { combination =>
      makeState(combination)
    }
  }

  def makeState(booleanCombination: List[Boolean]): ConcreteBooleanState = {
    GenericState(variables.zip(booleanCombination).toMap)
  }

}
