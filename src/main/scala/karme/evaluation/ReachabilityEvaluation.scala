package karme.evaluation

import java.io.File

import karme.Experiments
import karme.graphs.StateGraphs
import karme.printing.SynthesisResultLogger
import karme.simulation.AsyncBooleanNetworkSimulation
import karme.synthesis.SynthesisResult
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.visualization.StateGraphVisualization

/**
  * Evaluates combinations of synthesis results by simulating functions and
  * comparing to the original observed states.
  */
object ReachabilityEvaluation {

  def evaluate(
    labelToSynthesisResults: Map[String, Set[SynthesisResult]],
    initialStates: Set[ConcreteBooleanState],
    observedStates: Set[ConcreteBooleanState],
    outFolder: File
  ): Unit = {
    // first enumerate all combinations of synthesis results
    val combinations =
      SynthesisResultEnumeration.enumerateSynthesisResultCombinations(
        labelToSynthesisResults)

    // collect function combination, simulated states, and simulation penalty
    val resultTuples = for (labelToResult <- combinations) yield {
      val labelToFun = labelToResult map {
        case (l, r) => (l, r.functions.head)
      }
      val simulatedStates =
        AsyncBooleanNetworkSimulation.simulate(labelToFun, initialStates)

      val penalty = simulationPenalty(observedStates, simulatedStates)

      (labelToResult, simulatedStates, penalty)
    }

    // order by increasing penalty to print and plot
    for (((labelToResult, simulated, penalty), i) <-
         resultTuples.toSeq.sortBy(_ ._3).zipWithIndex) {
      println(s"Combination $i:")
      println(s"Penalty: $penalty")

      // print functions into file
      SynthesisResultLogger(labelToResult,
        new File(outFolder, s"functions-$i.txt"))

      // transfer code that plots simulated graph
      plotSimulation(initialStates, observedStates, simulated,
        s"simulation-${i}", outFolder)
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
