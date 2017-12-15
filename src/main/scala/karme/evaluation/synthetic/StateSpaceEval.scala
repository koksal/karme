package karme.evaluation.synthetic

import karme.Reporter
import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.simulation.AsyncBooleanNetworkSimulation
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.Transitions.ConcreteBooleanState

object StateSpaceEval {

  def compareStateSpaces(
    originalGraph: DirectedBooleanStateGraph,
    inferredModel: Map[String, FunExpr],
    initialStates: Set[ConcreteBooleanState]
  )(implicit reporter: Reporter) = {
    val (simGraphFromInferredModel, _) = AsyncBooleanNetworkSimulation
      .simulateOneStepWithStateGraph(inferredModel, initialStates)

    GraphComparison.diffGraphs(originalGraph, simGraphFromInferredModel)
  }

  def headers: Seq[String] = GraphComparison.headers

}
