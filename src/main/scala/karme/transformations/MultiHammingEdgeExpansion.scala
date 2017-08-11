package karme.transformations

import karme.graphs.Graphs.EdgeDirection
import karme.graphs.Graphs.UnlabeledEdge
import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.graphs.StateGraphs.StateGraphVertex

object MultiHammingEdgeExpansion {

  def expandMultiHammingEdges(
    g: DirectedBooleanStateGraph
  ): DirectedBooleanStateGraph = {
    // produce all orders of 1-Hamming switches
    // for each order
    //   for each intermediary state along the order
    //     add "empty" state if it doesn't exist
    //     add 1-Hamming link from the previous node

    for {
      e <- g.E
      dir <- g.edgeDirections(e)
    } {
      val seqs = allSequences(e, dir)
    }

    ???
  }

  private def allSequences(
    e: UnlabeledEdge[StateGraphVertex],
    dir: EdgeDirection
  ): Seq[Seq[StateGraphVertex]] = {
    // gather before/after values for each label
    // apply all serial combinations of value change
    ???
  }

}
