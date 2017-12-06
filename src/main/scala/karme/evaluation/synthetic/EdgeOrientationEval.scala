package karme.evaluation.synthetic

import karme.CellTrajectories.CellTrajectory
import karme.Reporter
import karme.graphs.Graphs.Backward
import karme.graphs.Graphs.Forward
import karme.graphs.Graphs.UnlabeledEdge
import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.graphs.StateGraphs.StateGraphVertex
import karme.transformations.DistributionComparisonTest
import karme.transformations.NodePartialOrderByPseudotimePartialOrder
import karme.visualization.graph.StateGraphPlotter

class EdgeOrientationEval(implicit reporter: Reporter) {

  def evaluateOrientation(
    simulationGraph: DirectedBooleanStateGraph,
    trajectories: Seq[CellTrajectory],
    distributionComparisonTest: DistributionComparisonTest,
    pValueThreshold: Double
  ) = {
    val partialOrdering = new NodePartialOrderByPseudotimePartialOrder(
      simulationGraph.V.toSeq, trajectories, distributionComparisonTest,
      pValueThreshold).partialOrdering

    var nbCorrectOrientation = 0
    var nbOppositeOrientation = 0
    var nbInconclusiveOrientation = 0
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
        nbCorrectOrientation += 1
        correctOrientationE += e
      } else if (partialOrdering.lt(toNode, fromNode)) {
        nbOppositeOrientation += 1
        oppositeOrientationE += e
      } else {
        nbInconclusiveOrientation += 1
        inconclusiveOrientationE += e
      }
    }

    new StateGraphPlotter(reporter).plotDirectedGraph(
      simulationGraph,
      "simulation-graph-with-orientation-colors",
      edgeHighlightGroups = List(
        correctOrientationE,
        inconclusiveOrientationE,
        oppositeOrientationE
      )
    )

    Map(
      "Nb. correct orientations" -> nbCorrectOrientation,
      "Nb. opposite orientations" -> nbOppositeOrientation,
      "Nb. inconclusive orientations" -> nbInconclusiveOrientation
    )
  }

}
