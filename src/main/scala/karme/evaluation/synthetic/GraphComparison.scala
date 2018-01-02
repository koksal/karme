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

    var nbCapturedEdges = 0
    var nbMissedEdges = 0
    var nbUnobservedEdges = 0
    var nbCapturedOrientations = 0
    var nbMissedOrientations = 0

    // join edges by the pair of states for endpoints.
    for (statePairSet <-
         statePairSetToGraph1Edges.keySet ++ statePairSetToGraph2Edges.keySet) {
      (
        statePairSetToGraph1Edges.get(statePairSet),
        statePairSetToGraph2Edges.get(statePairSet)
      ) match {
        case (Some(es1), Some(es2)) => {
          nbCapturedEdges += 1

          assert(es1.size == 1 && es2.size == 1)
          val ds1 = originalGraph.edgeDirections(es1.head)
          val ds2 = inferredGraph.edgeDirections(es2.head)
          val sameEdgeOrder = es1.head.v1.state == es2.head.v1.state
          if (sameEdgeOrder) {
            if (ds1.subsetOf(ds2)) {
              nbCapturedOrientations += 1
            } else {
              nbMissedOrientations += 1
            }
          } else {
            if (ds1.subsetOf(ds2.map(Graphs.reverseDirection))) {
              nbCapturedOrientations += 1
            } else {
              nbMissedOrientations += 1
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
      "Captured V" -> states1.intersect(states2).size,
      "Missed V" -> (states1 -- states2).size,
      "Spurious V" -> (states2 -- states1).size,
      "Captured E" -> nbCapturedEdges,
      "Missed E" -> nbMissedEdges,
      "Spurious E" -> nbUnobservedEdges,
      "Captured D" -> nbCapturedOrientations,
      "Missed D" -> nbMissedOrientations
    )
  }

  def headers: Seq[String] = {
    List(
      "Captured V",
      "Missed V",
      "Spurious V",
      "Captured E",
      "Missed E",
      "Spurious E",
      "Captured D",
      "Missed D"
    )
  }

}
