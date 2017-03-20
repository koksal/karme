package karme.evaluation

import java.io.File

import karme.Experiments
import karme.Reporter
import karme.graphs.StateGraphs
import karme.simulation.AsyncBooleanNetworkSimulation
import karme.synthesis.SynthesisResult
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.visualization.StateGraphVisualization

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

  def chooseOptimalReachabilityResult(
    labelToSynthesisResults: Map[String, Set[SynthesisResult]],
    initialStates: Set[ConcreteBooleanState],
    observedStates: Set[ConcreteBooleanState],
    reporter: Reporter
  ): ReachabilityEvaluationResult = {
    val results = computeReachabilityEvaluationResults(labelToSynthesisResults,
      initialStates, observedStates)

    // TODO be aware of the arbitrary tie-break between equal-penalty cases
    results.minBy(_.simulationPenalty)
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

  private def plotSimulation(
    initialStates: Set[ConcreteBooleanState],
    observedStates: Set[ConcreteBooleanState],
    simulatedStates: Set[ConcreteBooleanState],
    name: String,
    outFolder: File
  ): Unit = {
    val unionExp = Experiments.booleanStatesToExperiment(
      observedStates ++ simulatedStates)
    // TODO carry the Hamming distance here
    val unionGraph = StateGraphs.fromBooleanExperiment(unionExp, 1)

    val missedStates = observedStates -- simulatedStates
    val unobservedStates = simulatedStates -- observedStates
    val highlightGroups = List(initialStates, unobservedStates, missedStates)

    StateGraphVisualization.plotUndirectedGraph(unionGraph, name, outFolder,
      nodeHighlightGroups = highlightGroups)
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
