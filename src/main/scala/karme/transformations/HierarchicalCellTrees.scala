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

object HierarchicalCellTrees {

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

}
