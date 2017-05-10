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

  val trajectoryToPValues: Map[CellTrajectory, Seq[Seq[Option[Double]]]] = {
    val pairs = trajectories map { t =>
      t -> trajectoryPValues(t)
    }

    pairs.toMap
  }

  def trajectoryPValues(t: CellTrajectory): Seq[Seq[Option[Double]]] = {
    for (i <- 0 until V.size) yield {
      for (j <- 0 until V.size) yield {
        computeRankSumIfNonEmptySamples(
          StateGraphs.nodePseudotimes(V(i), t),
          StateGraphs.nodePseudotimes(V(j), t)
        )
      }
    }
  }

  def computeRankSumIfNonEmptySamples(
    leftPseudotimes: Seq[Double], rightPseudotimes: Seq[Double]
  ): Option[Double] = {
    if (leftPseudotimes.isEmpty || rightPseudotimes.isEmpty) {
      None
    } else {
      val res = new RankSumTest(rightPseudotimes, leftPseudotimes).run()
      Some(res.pValue)
    }
  }

  def comparisonFunction: (StateGraphVertex, StateGraphVertex) => Boolean = {
    def lt(v1: StateGraphVertex, v2: StateGraphVertex): Boolean = {
      val i = V.indexOf(v1)
      val j = V.indexOf(v2)

      // at least one trajectory must be able to cover both nodes
      assert(trajectories.exists(t => trajectoryToPValues(t)(i)(j).nonEmpty))

      // if a trajectory covers both nodes, the p-value must be significant
      trajectories.forall { t =>
        trajectoryToPValues(t)(i)(j) match {
          case None => true
          case Some(pVal) => pVal < P_VALUE_THRESHOLD
        }
      }
    }

    lt
  }

}
