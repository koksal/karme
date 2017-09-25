package karme.evaluation.synthetic.stategen
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.synthesis.Transitions.GenericState
import karme.util.MathUtil

class ExhaustiveStateEnumeration(
  variables: Seq[String]
) extends StateEnumeration {

  def enumerateInitialStates(): Seq[Set[ConcreteBooleanState]] = {
    val setsForProduct = variables map {
      v => Set(true, false)
    }

    val product = MathUtil.cartesianProduct(setsForProduct.toList)

    product.toList map { combination =>
      Set(makeState(combination))
    }
  }

  def makeState(booleanCombination: List[Boolean]): ConcreteBooleanState = {
    GenericState(variables.zip(booleanCombination).toMap)
  }

}
