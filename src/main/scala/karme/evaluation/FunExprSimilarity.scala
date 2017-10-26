package karme.evaluation

import karme.evaluation.synthetic.stategen.ExhaustiveStateEnumeration
import karme.synthesis.FunctionTrees
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.Transitions.ConcreteBooleanState

object FunExprSimilarity {

  def commonBehaviorRatio(e1: FunExpr, e2: FunExpr): Double = {
    val allIdentifiers = FunctionTrees.collectIdentifiers(e1)
      .union(FunctionTrees.collectIdentifiers(e2))

    val allStates = new ExhaustiveStateEnumeration(allIdentifiers.toSeq)
      .enumerateAllStates()

    val agreeingInputs = allStates.count(s => functionsAgreeOnInput(e1, e2, s))

    agreeingInputs.toDouble / allStates.size
  }

  private def functionsAgreeOnInput(
    e1: FunExpr, e2: FunExpr, s: ConcreteBooleanState
  ): Boolean = {
    FunctionTrees.eval(e1, s) == FunctionTrees.eval(e2, s)
  }

}
