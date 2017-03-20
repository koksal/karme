package karme.synthesis

import karme.Reporter
import karme.SynthOpts
import karme.synthesis.FunctionTrees.FunVar
import karme.synthesis.Transitions.GenericState
import karme.synthesis.Transitions.Transition
import org.scalatest.FunSuite

class SynthesisTest extends FunSuite {

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

    // TODO refactor Synthesizer so we don't need to instantiate here with
    // the irrelevant maxExpressionDepth
    val synthesizer = new Synthesizer(SynthOpts(maxExpressionDepth = 5,
      maxNbModels = None), Reporter.defaultReporter())
    val res = synthesizer.enumerateFunExprForMinNbVars(transitions,
      possibleVars, 5)

    assertResult(List(FunVar("B")))(res)
  }

}
