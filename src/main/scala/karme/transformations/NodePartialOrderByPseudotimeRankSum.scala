package karme.transformations

import karme.CellTrajectories.CellTrajectory
import karme.graphs.StateGraphs
import karme.graphs.StateGraphs.StateGraphVertex

class NodePartialOrderByPseudotimeRankSum(
  V: Seq[StateGraphVertex],
  trajectories: Seq[CellTrajectory],
  distributionComparisonTest: DistributionComparisonTest
) {

  val P_VALUE_THRESHOLD = 0.05

  private var pValueCache:
    Map[(CellTrajectory, String, String), Option[Double]] = Map.empty

  def partialOrdering: PartialOrdering[StateGraphVertex] = {
    new PartialOrdering[StateGraphVertex] {

      def tryCompare(x: StateGraphVertex, y: StateGraphVertex): Option[Int] = {
        // not comparable if nodes do not share a trajectory
        if (!nodesShareTrajectory(x, y)) {
          None
        } else {
          if (this.lt(x, y)) {
            Some(-1)
          } else if (this.equiv(x, y)) {
            Some(0)
          } else {
            assert(this.gt(x, y))
            Some(1)
          }
        }
      }

      def lteq(x: StateGraphVertex, y: StateGraphVertex): Boolean = {
        nodesShareTrajectory(x, y) &&
          trajectories.forall(t => !significantGT(x, y, t))
      }

    }
  }

  def gtPValue(
    x: StateGraphVertex, y: StateGraphVertex, t: CellTrajectory
  ): Option[Double] = {
    this.synchronized {
      val key = (t, x.id, y.id)

      pValueCache.get(key) match {
        case None => {
          // compute and cache
          val pv = computeRankSumIfNonEmptySamples(
            StateGraphs.nodePseudotimes(x, t),
            StateGraphs.nodePseudotimes(y, t)
          )
          pValueCache += key -> pv
          pv
        }
        case Some(pv) => pv
      }
    }
  }

  def significantGT(
    x: StateGraphVertex, y: StateGraphVertex, t: CellTrajectory
  ): Boolean = gtPValue(x, y, t) match {
    case Some(v) => v <= P_VALUE_THRESHOLD
    case None => false
  }

  private def nodesShareTrajectory(
    x: StateGraphVertex, y: StateGraphVertex
  ): Boolean = {
    trajectories exists { t =>
      gtPValue(x, y, t).nonEmpty
    }
  }

  private def computeRankSumIfNonEmptySamples(
    leftPseudotimes: Seq[Double], rightPseudotimes: Seq[Double]
  ): Option[Double] = {
    if (leftPseudotimes.isEmpty || rightPseudotimes.isEmpty) {
      None
    } else {
      val pValue = distributionComparisonTest.testPValue(leftPseudotimes,
        rightPseudotimes)
      Some(pValue)
    }
  }

}
