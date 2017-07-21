package karme.graphs

import karme.CellTrajectories.CellTrajectory
import karme.graphs.Graphs.Backward
import karme.graphs.Graphs.EdgeDirection
import karme.graphs.Graphs.Forward
import karme.graphs.Graphs.UnlabeledEdge
import karme.graphs.StateGraphs.StateGraphVertex
import karme.graphs.StateGraphs.UndirectedBooleanStateGraph
import karme.transformations.RankSumTest

class GraphOrientationByTrajectory(g: UndirectedBooleanStateGraph) {

  private val rankSum = new RankSumTest

  def orientForTrajectory(trajectory: CellTrajectory): Map[
    UnlabeledEdge[StateGraphVertex], Set[EdgeDirection]] = {

    var res = Map[UnlabeledEdge[StateGraphVertex], Set[EdgeDirection]]()

    for (e @ UnlabeledEdge(v1, v2) <- g.E) {
      val dirOpt = orientByRankSum(
        StateGraphs.nodePseudotimes(v1, trajectory),
        StateGraphs.nodePseudotimes(v2, trajectory)
      )
      dirOpt foreach {
        res += e -> Set(_)
      }
    }

    res
  }

  private def orientByRankSum(
    leftPseudotimes: Seq[Double], rightPseudotimes: Seq[Double]
  ): Option[EdgeDirection] = {
    val P_VALUE_THRESHOLD = 0.05

    if (leftPseudotimes.isEmpty || rightPseudotimes.isEmpty) {
      None
    } else {
      val forwardPVal = rankSum.test(rightPseudotimes, leftPseudotimes).pValue
      val backwardPVal = rankSum.test(leftPseudotimes, rightPseudotimes).pValue

      if (forwardPVal <= P_VALUE_THRESHOLD) {
        assert(backwardPVal > P_VALUE_THRESHOLD)
        Some(Forward)
      } else if (backwardPVal <= P_VALUE_THRESHOLD) {
        Some(Backward)
      } else {
        None
      }
    }
  }
}
