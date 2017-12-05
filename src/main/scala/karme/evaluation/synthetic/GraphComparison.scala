package karme.evaluation.synthetic

import karme.graphs.Graphs.Backward
import karme.graphs.Graphs.EdgeDirection
import karme.graphs.Graphs.Forward
import karme.graphs.StateGraphs.DirectedBooleanStateGraph

class GraphComparison {

  def diffGraphs(
    g1: DirectedBooleanStateGraph, g2: DirectedBooleanStateGraph
  ): Map[String, Any] = {
    def reverse(d: EdgeDirection): EdgeDirection = d match {
      case Forward => Backward
      case Backward => Forward
    }

    val states1 = g1.V.map(_.state)
    val states2 = g2.V.map(_.state)

    val statePairSetToGraph1Edges = g1.E.groupBy(
      e => Set(e.v1.state, e.v2.state))
    val statePairSetToGraph2Edges = g2.E.groupBy(
      e => Set(e.v1.state, e.v2.state))

    var nbMissedEdges = 0
    var nbUnobservedEdges = 0
    var nbOrigDirectionCaptured = 0
    var nbOrigDirectionNonCaptured = 0

    for (statePairSet <-
         statePairSetToGraph1Edges.keySet ++ statePairSetToGraph2Edges.keySet) {
      (
        statePairSetToGraph1Edges.get(statePairSet),
        statePairSetToGraph2Edges.get(statePairSet)
      ) match {
        case (Some(es1), Some(es2)) => {
          assert(es1.size == 1 && es2.size == 1)
          val ds1 = g1.edgeDirections(es1.head)
          val ds2 = g2.edgeDirections(es2.head)
          val sameEdgeOrder = es1.head.v1.state == es2.head.v1.state
          if (sameEdgeOrder) {
            if (ds1.subsetOf(ds2)) {
              nbOrigDirectionCaptured += 1
            } else {
              nbOrigDirectionNonCaptured += 1
            }
          } else {
            if (ds1.subsetOf(ds2.map(reverse))) {
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
      "States only in 1" -> (states1 -- states2).size,
      "States only in 2" -> (states2 -- states1).size,
      "Captured directions" -> nbOrigDirectionCaptured,
      "Non-captured directions" -> nbOrigDirectionNonCaptured,
      "Missed edges" -> nbMissedEdges,
      "Unobserved edges" -> nbUnobservedEdges
    )
  }

}
