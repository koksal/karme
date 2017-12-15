package karme.evaluation.synthetic

import karme.graphs.Graphs
import karme.graphs.StateGraphs.DirectedBooleanStateGraph

object GraphComparison {

  def diffGraphs(
    originalGraph: DirectedBooleanStateGraph,
    inferredGraph: DirectedBooleanStateGraph
  ): Map[String, Any] = {
    val states1 = originalGraph.V.map(_.state)
    val states2 = inferredGraph.V.map(_.state)

    val statePairSetToGraph1Edges = originalGraph.E.groupBy(
      e => Set(e.v1.state, e.v2.state))
    val statePairSetToGraph2Edges = inferredGraph.E.groupBy(
      e => Set(e.v1.state, e.v2.state))

    var nbMissedEdges = 0
    var nbUnobservedEdges = 0
    var nbOrigDirectionCaptured = 0
    var nbOrigDirectionNonCaptured = 0

    // join edges by the pair of states for endpoints.
    for (statePairSet <-
         statePairSetToGraph1Edges.keySet ++ statePairSetToGraph2Edges.keySet) {
      (
        statePairSetToGraph1Edges.get(statePairSet),
        statePairSetToGraph2Edges.get(statePairSet)
      ) match {
        case (Some(es1), Some(es2)) => {
          assert(es1.size == 1 && es2.size == 1)
          val ds1 = originalGraph.edgeDirections(es1.head)
          val ds2 = inferredGraph.edgeDirections(es2.head)
          val sameEdgeOrder = es1.head.v1.state == es2.head.v1.state
          if (sameEdgeOrder) {
            if (ds1.subsetOf(ds2)) {
              nbOrigDirectionCaptured += 1
            } else {
              nbOrigDirectionNonCaptured += 1
            }
          } else {
            if (ds1.subsetOf(ds2.map(Graphs.reverseDirection))) {
              nbOrigDirectionCaptured += 1
            } else {
              nbOrigDirectionNonCaptured += 1
            }
          }
        }
        case (Some(_), None) => {
          nbMissedEdges += 1
        }
        case (None, Some(_)) => {
          nbUnobservedEdges += 1
        }
        case _ => throw new Exception("Cannot happen.")
      }
    }

    Map(
      "Missed states" -> (states1 -- states2).size,
      "Spurious states" -> (states2 -- states1).size,
      "Missed edges" -> nbMissedEdges,
      "Spurious edges" -> nbUnobservedEdges,
      "Captured dir." -> nbOrigDirectionCaptured,
      "Missed dir." -> nbOrigDirectionNonCaptured
    )
  }


  def headers: Seq[String] = {
    List(
      "Missed states",
      "Spurious states",
      "Missed edges",
      "Spurious edges",
      "Captured dir.",
      "Missed dir."
    )
  }

}
