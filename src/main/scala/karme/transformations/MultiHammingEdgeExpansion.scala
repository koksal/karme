package karme.transformations

import karme.graphs.Graphs.EdgeDirection
import karme.graphs.Graphs.UnlabeledEdge
import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.graphs.StateGraphs.StateGraphVertex
import karme.graphs.StateGraphs.UndirectedStateGraphOps

class MultiHammingEdgeExpansion(g: DirectedBooleanStateGraph) {

  def expandMultiHammingEdges(): DirectedBooleanStateGraph = {
    // produce all orders of 1-Hamming switches
    // for each order
    //   for each intermediary state along the order
    //     add "empty" state if it doesn't exist
    //     add 1-Hamming link from the previous node

    for {
      e <- g.E
      dir <- g.edgeDirections(e)
    } {
      val labels = UndirectedStateGraphOps.edgeLabels(e)
      val seqs = allSequences(labels, g.source(e, dir), g.target(e, dir))
    }

    ???
  }

  private def allSequences(
    differingNames: Seq[String],
    source: StateGraphVertex,
    target: StateGraphVertex
  ): Seq[Seq[StateGraphVertex]] = {
    // gather before/after values for each label
    val nameOrderings = allNameOrderings(differingNames)

    // apply all serial combinations of value change

    // if the graph doesn't have a node with the new state, add new state
    // with empty measurements

    ???
  }

  private def allNameOrderings(names: Seq[String]): Seq[Seq[String]] = {
    if (names.isEmpty) {
      Seq(Seq())
    } else {
      (0 until names.size).flatMap { i =>
        val firstElem = names(i)
        val restElems = names.take(i) ++ names.drop(i + 1)
        val restOrderings = allNameOrderings(restElems)
        restOrderings map {
          o => firstElem +: o
        }
      }
    }
  }

}
