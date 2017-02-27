package karme.evaluation

import karme.simulation.AsyncBooleanNetworkSimulation
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.SynthesisResult
import karme.synthesis.Transitions.ConcreteBooleanState

/**
  * Evaluates combinations of synthesis results by simulating functions and
  * comparing to the original observed states.
  */
object ReachabilityEvaluation {

  def reachabilityPenalties(
    labelToSynthesisResults: Map[String, Set[SynthesisResult]],
    initialStates: Set[ConcreteBooleanState],
    observedStates: Set[ConcreteBooleanState]
  ): Seq[(Map[String, FunExpr], Double)] = {
    // first enumerate all combinations of synthesis results
    val combinations =
      SynthesisResultEnumeration.enumerateSynthesisResultCombinations(
        labelToSynthesisResults)

    // then compute reachability for each combination
    val combinationPenaltyPairs = for (labelToFunExpr <- combinations) yield {
      val simulatedStates =
        AsyncBooleanNetworkSimulation.simulate(labelToFunExpr, initialStates)

      (labelToFunExpr, reachabilityPenalty(observedStates, simulatedStates))
    }

    combinationPenaltyPairs.toSeq.sortBy(_._2)
  }

  def reachabilityPenalty(
    observedStates: Set[ConcreteBooleanState],
    simulatedStates: Set[ConcreteBooleanState]
  ): Double = {
    val missedStates = observedStates -- simulatedStates
    val unobservedStates = simulatedStates -- observedStates

    missedStates.size + unobservedStates.size
  }

}
