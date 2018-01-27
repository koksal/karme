package karme.synthesis

import karme.synthesis.FunctionTrees.{FunConst, FunVar}
import karme.synthesis.Transitions.{GenericState, Transition}
import karme.{Reporter, SynthOpts}
import org.scalatest.FunSuite

class SynthesizerTest extends FunSuite {

  test("Function expression enumeration for minimum number of variables") {
    val transitions = List(
      Transition(
        input = GenericState(Map("A" -> false, "B" -> false)),
        output = false,
        label = "A",
        weight = 0
      ),
      Transition(
        input = GenericState(Map("A" -> false, "B" -> true)),
        output = true,
        label = "A",
        weight = 0
      )
    )
    val possibleVars = Set("A", "B")
    val exprDepth = 0

    // TODO refactor Synthesizer so we don't need to instantiate here with
    // the irrelevant maxExpressionDepth
    val synthesizer = new Synthesizer(SynthOpts(maxExpressionDepth = exprDepth,
      maxNbModels = None), Reporter.defaultReporter())
    val res = synthesizer.enumerateFunExprForMinNbVars(transitions,
      possibleVars, exprDepth)

    assertResult(List(FunVar("B")))(res.toList)
  }

  private def testConstant(v: Boolean): Unit = {
    val transitions = List(
      Transition(
        input = GenericState(Map("A" -> false, "B" -> false)),
        output = v,
        label = "A",
        weight = 0
      ),
      Transition(
        input = GenericState(Map("A" -> false, "B" -> true)),
        output = v,
        label = "A",
        weight = 0
      )
    )
    val possibleVars = Set("A", "B")
    val exprDepth = 0

    // TODO refactor Synthesizer so we don't need to instantiate here with
    // the irrelevant maxExpressionDepth
    val synthesizer = new Synthesizer(SynthOpts(maxExpressionDepth = exprDepth,
      maxNbModels = None), Reporter.defaultReporter())
    val res = synthesizer.enumerateFunExprForMinNbVars(transitions,
      possibleVars, exprDepth)

    assertResult(List(FunConst(v)))(res.toList)
  }

  test("Constant function synthesis") {
    testConstant(true)
    testConstant(false)
  }
}
