package karme.transformations

import karme.CellTrajectories.CellTrajectory
import karme.evaluation.RankSumTest
import karme.graphs.StateGraphs
import karme.graphs.StateGraphs.{StateGraphVertex, UndirectedStateGraphOps}

class NodeComparisonByPseudotimeRankSum(
  V: Seq[StateGraphVertex],
  trajectories: Seq[CellTrajectory]
) {

  val P_VALUE_THRESHOLD = 0.05

  private val trajectoryToGTPValues: Map[
    CellTrajectory, Seq[Seq[Option[Double]]]] = {
    val pairs = trajectories map { t =>
      t -> trajectoryPValues(t)
    }

    pairs.toMap
  }

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
    val xi = V.indexOf(x)
    val yi = V.indexOf(y)

    trajectoryToGTPValues(t)(xi)(yi)
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

  private def trajectoryPValues(t: CellTrajectory): Seq[Seq[Option[Double]]] = {
    for (i <- 0 until V.size) yield {
      for (j <- 0 until V.size) yield {
        computeRankSumIfNonEmptySamples(
          StateGraphs.nodePseudotimes(V(i), t),
          StateGraphs.nodePseudotimes(V(j), t)
        )
      }
    }
  }

  private def computeRankSumIfNonEmptySamples(
    leftPseudotimes: Seq[Double], rightPseudotimes: Seq[Double]
  ): Option[Double] = {
    if (leftPseudotimes.isEmpty || rightPseudotimes.isEmpty) {
      None
    } else {
      val res = new RankSumTest(leftPseudotimes, rightPseudotimes).run()
      Some(res.pValue)
    }
  }

}
