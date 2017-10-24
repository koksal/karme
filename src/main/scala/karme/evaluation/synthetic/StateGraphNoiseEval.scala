package karme.evaluation.synthetic

import karme.Opts
import karme.Reporter
import karme.evaluation.synthetic.examples.CAVModel
import karme.evaluation.synthetic.examples.CAVModelEvaluation
import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.printing.SynthesisResultLogger
import karme.simulation.AsyncBooleanNetworkSimulation
import karme.synthesis.SynthesisResult
import karme.synthesis.Synthesizer
import karme.visualization.graph.StateGraphPlotter

import scala.util.Random

class StateGraphNoiseEval(opts: Opts, reporter: Reporter) {

  val random = new Random()

  def evaluateCAVModelPerturbations(): Unit = {
    val simulatedGraph = AsyncBooleanNetworkSimulation
      .simulateOneStepWithStateGraph(
        CAVModel.makeNetwork(), Set(CAVModel.makeInitialState()))

    val noisyStateGraph = deleteNodes(simulatedGraph, 0.01)

    // todo are stable states still reachable?

    // todo measure synthesis time
    val results = new Synthesizer(opts.synthOpts, reporter)
      .synthesizeForPositiveHardConstraints(noisyStateGraph)
    SynthesisResultLogger(results, reporter.file("functions.txt"))

    val resultCombinations = SynthesisResult.makeCombinations(results)
    for ((resultCombination, i) <- resultCombinations.zipWithIndex) yield {
      println(s"Testing inferred model $i:")
      CAVModelEvaluation.evaluateModelBehavior(resultCombination)
    }
  }

  def deleteNodes(
    stateGraph: DirectedBooleanStateGraph,
    nodeDeletionRatio: Double
  ) = {
    var newGraph = stateGraph

    val nbNodesToDelete = (stateGraph.V.size * nodeDeletionRatio).toInt
    val nodesToDelete = random.shuffle(stateGraph.V.toList)
      .take(nbNodesToDelete)

    for (nodeToDelete <- nodesToDelete) {
      newGraph = newGraph.removeVertex(nodeToDelete)
    }

    newGraph
  }

}
