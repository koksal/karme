package karme.synthesis

import karme.synthesis.Trees.{And, GreaterEquals, GreaterThan, IntLiteral}
import org.scalatest.FunSuite

class OptimizationTest extends FunSuite {

  test("optimize for positive value") {
    val v1 = Trees.mkFreshIntVar("v1")
    val v2 = Trees.mkFreshIntVar("v2")
    val constraints = And(
      GreaterEquals(v1, v2),
      GreaterThan(v2, IntLiteral(42))
    )

    val expected = Some(43)
    val res = Optimization.minimize(v1.id, constraints)

    assertResult(expected)(res)
  }

  test("optimize for negative value") {
    val v1 = Trees.mkFreshIntVar("v1")
    val v2 = Trees.mkFreshIntVar("v2")
    val constraints = And(
      GreaterEquals(v1, v2),
      GreaterThan(v2, IntLiteral(-42))
    )

    val expected = Some(-41)
    val res = Optimization.minimize(v1.id, constraints)

    assertResult(expected)(res)
  }
}
