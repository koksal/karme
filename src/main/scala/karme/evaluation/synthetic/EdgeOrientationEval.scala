package karme.evaluation.synthetic

import karme.Reporter
import karme.graphs.Graphs.Backward
import karme.graphs.Graphs.Forward
import karme.graphs.Graphs.UnlabeledEdge
import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.graphs.StateGraphs.StateGraphVertex
import karme.visualization.graph.StateGraphPlotter

class EdgeOrientationEval(implicit reporter: Reporter) {

  private val visualizeOrientationSuccess = true

  def evaluateOrientation(
    simulationGraph: DirectedBooleanStateGraph,
    partialOrdering: PartialOrdering[StateGraphVertex]
  ) = {
    var correctOrientationE = Set[UnlabeledEdge[StateGraphVertex]]()
    var oppositeOrientationE = Set[UnlabeledEdge[StateGraphVertex]]()
    var inconclusiveOrientationE = Set[UnlabeledEdge[StateGraphVertex]]()

    for (e <- simulationGraph.E) {
      val ds = simulationGraph.edgeDirections(e)
      assert(ds.size == 1)
      val d = ds.head

      val (fromNode, toNode) =  d match {
        case Forward => (e.v1, e.v2)
        case Backward => (e.v2, e.v1)
      }

      if (partialOrdering.lt(fromNode, toNode)) {
        assert(!partialOrdering.lt(toNode, fromNode))
        correctOrientationE += e
      } else if (partialOrdering.lt(toNode, fromNode)) {
        oppositeOrientationE += e
      } else {
        inconclusiveOrientationE += e
      }
    }

    if (visualizeOrientationSuccess) {
      new StateGraphPlotter(reporter).plotDirectedGraph(
        simulationGraph,
        "simulation-graph-with-orientation-colors",
        edgeHighlightGroups = List(
          correctOrientationE,
          inconclusiveOrientationE,
          oppositeOrientationE
        )
      )
    }

    Map(
      "Correct" -> correctOrientationE.size,
      "Incorrect" -> oppositeOrientationE.size,
      "Inconclusive" -> inconclusiveOrientationE.size
    )
  }

}

object EdgeOrientationEval {

  def headers: Seq[String] = List(
    "Correct",
    "Incorrect",
    "Inconclusive"
  )

}
