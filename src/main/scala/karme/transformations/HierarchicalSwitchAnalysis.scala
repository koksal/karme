package karme.transformations

import karme.CellTrajectories
import karme.CellTrajectories.CellTrajectory
import karme.Experiments.Experiment
import karme.Experiments.Measurement

sealed trait HierarchicalCellTree

case class CellTreeNode(
  ms: Seq[Measurement[Double]],
  left: HierarchicalCellTree,
  right: HierarchicalCellTree
) extends HierarchicalCellTree

case class CellTreeLeaf(
  ms: Seq[Measurement[Double]]
) extends HierarchicalCellTree

class HierarchicalSwitchAnalysis(
  experiment: Experiment[Double],
  trajectory: CellTrajectory
) {

  private val P_VALUE_THRESHOLD = 0.001

  private val orderedExperiment =
    CellTrajectories.orderMeasurementsByTrajectory(experiment, trajectory)

  private val ksTest = new KolmogorovSmirnovTest()

  def buildCellHierarchy(ms: Seq[Measurement[Double]]): HierarchicalCellTree = {
    if (ms.size < 2) {
      CellTreeLeaf(ms)
    } else {
      val leftHalf = ms.take(ms.size / 2)
      val rightHalf = ms.drop(ms.size / 2)
      CellTreeNode(
        ms,
        buildCellHierarchy(leftHalf),
        buildCellHierarchy(rightHalf)
      )
    }
  }

  def height(cellTree: HierarchicalCellTree): Int = cellTree match {
    case CellTreeNode(_, l, r) => {
      math.max(height(l), height(r)) + 1
    }
    case CellTreeLeaf(_) => {
      0
    }
  }

  def findMeasurementSetsAtLevel(
    cellTree: HierarchicalCellTree, level: Int
  ): Seq[Seq[Measurement[Double]]] = cellTree match {
    case CellTreeNode(ms, l, r) => {
      if (level == 0) {
        Seq(ms)
      } else {
        val leftSets = findMeasurementSetsAtLevel(l, level - 1)
        val rightSets = findMeasurementSetsAtLevel(r, level - 1)
        leftSets ++ rightSets
      }
    }
    case CellTreeLeaf(ms) => {
      if (level == 0) {
        Seq(ms)
      } else {
        Seq()
      }
    }
  }

  def analyzeGenes(): Unit = {
    val cellTree = buildCellHierarchy(orderedExperiment.measurements)
    val cellTreeHeight = height(cellTree)

    for (name <- experiment.names) {
      println(s"Analyzing gene $name")

      for (i <- 1 until cellTreeHeight) {
        val measurementClustersAtHeight = findMeasurementSetsAtLevel(cellTree, i)
        val values = measurementClustersAtHeight map (
          ms => ms.map(m => m.state.value(name)))

        val valuePairs = values.zip(values.tail)

        print(s"Level $i: ")

        for ((vs1, vs2) <- valuePairs) {
          val upreg = isUpregulation(vs1, vs2)
          upreg match {
            case Some(true) => print("u")
            case Some(false) => print("d")
            case None => print("-")
          }
        }

        println()
      }
    }
  }

  def isUpregulation(
    leftVs: Seq[Double], rightVs: Seq[Double]
  ): Option[Boolean] = {
    val decreases = ksTest.getPValue(leftVs, rightVs) < P_VALUE_THRESHOLD
    val increases = ksTest.getPValue(rightVs, leftVs) < P_VALUE_THRESHOLD

    assert(!(decreases && increases))

    if (!increases && !decreases) {
      None
    } else {
      Some(increases)
    }
  }

}
