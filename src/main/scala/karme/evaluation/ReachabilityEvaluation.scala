package karme.evaluation

import karme.Experiments
import karme.Reporter
import karme.graphs.StateGraphs
import karme.simulation.AsyncBooleanNetworkSimulation
import karme.synthesis.SynthesisResult
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.util.MathUtil
import karme.visualization.StateGraphPlotter

/**
  * Evaluates combinations of synthesis results by simulating functions and
  * comparing to the original observed states.
  */
object ReachabilityEvaluation {

  case class ReachabilityEvaluationResult(
    labelToResult: Map[String, SynthesisResult],
    reachableStates: Set[ConcreteBooleanState],
    simulationPenalty: Double
  )

  def findAllResultsWithOptimalReachability(
    labelToSynthesisResults: Map[String, Set[SynthesisResult]],
    initialStates: Set[ConcreteBooleanState],
    observedStates: Set[ConcreteBooleanState],
    reporter: Reporter
  ): Seq[ReachabilityEvaluationResult] = {
    val results = computeReachabilityEvaluationResults(labelToSynthesisResults,
      initialStates, observedStates)

    // return all results with minimum penalty
    val minScore = results.map(_.simulationPenalty).min
    val scoreEpsilon = 0.1
    results filter { r =>
      MathUtil.approxEquals(precision = scoreEpsilon)(r.simulationPenalty,
        minScore)
    }
  }

  def computeReachabilityEvaluationResults(
    labelToSynthesisResults: Map[String, Set[SynthesisResult]],
    initialStates: Set[ConcreteBooleanState],
    observedStates: Set[ConcreteBooleanState]
  ): Seq[ReachabilityEvaluationResult] = {
    // first enumerate all combinations of synthesis results
    val combinations =
      SynthesisResultEnumeration.enumerateSynthesisResultCombinations(
        labelToSynthesisResults)

    // Build results
    for (labelToResult <- combinations.toSeq) yield {
      val labelToFun = labelToResult map {
        case (l, r) => (l, r.functions.head)
      }
      val simulatedStates =
        AsyncBooleanNetworkSimulation.simulate(labelToFun, initialStates)

      val penalty = simulationPenalty(observedStates, simulatedStates)

      ReachabilityEvaluationResult(labelToResult, simulatedStates, penalty)
    }
  }

  private def simulationPenalty(
    observedStates: Set[ConcreteBooleanState],
    simulatedStates: Set[ConcreteBooleanState]
  ): Double = {
    val missedStates = observedStates -- simulatedStates
    val unobservedStates = simulatedStates -- observedStates

    missedStates.size + unobservedStates.size
  }

}
