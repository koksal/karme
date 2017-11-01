package karme.evaluation

import karme.evaluation.synthetic.stategen.ExhaustiveStateEnumeration
import karme.synthesis.FunctionTrees
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.Transitions.ConcreteBooleanState

object FunExprSimilarity {

  def findNonRedundantSet(es: Set[FunExpr]): Set[FunExpr] = {
    // pick a function
    // find all functions equivalent to it
    // pick the simplest one among them for final set (e.g. least vars)

    var toProcess = es
    var nonRedundantSet = Set[FunExpr]()

    while (toProcess.nonEmpty) {
      val nextFun = toProcess.head
      toProcess -= nextFun
      val equivalentFuns = toProcess.filter(
        f => commonBehaviorRatio(nextFun, f) == 1.0)

      val allEquivalentFuns = equivalentFuns + nextFun

      val simplestEquivalentFun = allEquivalentFuns.minBy(
        f => FunctionTrees.collectIdentifiers(f).size)

      nonRedundantSet += simplestEquivalentFun
      toProcess --= allEquivalentFuns
    }

    if (nonRedundantSet.size < es.size) {
      println(s"Reduced ${es.size} functions to a non-redundant set of " +
        s"${nonRedundantSet.size}")
    }
    nonRedundantSet
  }

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
