package karme.evaluation

import karme.synthesis.FunctionTrees.{FunAnd, FunOr, FunVar}
import org.scalatest.FunSuite

class FunExprSimilarityTest extends FunSuite {

  val a = "a"
  val b = "b"
  test("compare functions with same identifier set") {
    val e1 = FunAnd(FunVar(a), FunVar(b))
    val e2 = FunOr(FunVar(a), FunVar(b))

    // agreement is 0.5, for a and b false, and a and b true
    assertResult(0.5)(FunExprSimilarity.commonBehaviorRatio(e1, e2))
  }
}
