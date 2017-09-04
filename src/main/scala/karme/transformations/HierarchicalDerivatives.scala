package karme.transformations

import karme.CellTrajectories
import karme.CellTrajectories.CellTrajectory
import karme.Experiments.Experiment

class HierarchicalDerivatives(
  experiment: Experiment[Double],
  trajectory: CellTrajectory
) {

  private val P_VALUE_THRESHOLD = 0.001

  private val orderedExperiment =
    CellTrajectories.orderMeasurementsByTrajectory(experiment, trajectory)

  private val distribComp: DistributionComparisonTest =
    new KolmogorovSmirnovTest()

  def analyzeGenes(): Unit = {
    val cellTree = HierarchicalCellTrees.buildCellHierarchy(
      orderedExperiment.measurements)
    val cellTreeHeight = HierarchicalCellTrees.height(cellTree)

    for (name <- experiment.names) {
      println(s"Analyzing gene $name")

      for (i <- 1 until cellTreeHeight) {
        val measurementClustersAtHeight =
          HierarchicalCellTrees.findMeasurementSetsAtLevel(cellTree, i)
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
    val decreases = distribComp.testPValue(leftVs, rightVs) < P_VALUE_THRESHOLD
    val increases = distribComp.testPValue(rightVs, leftVs) < P_VALUE_THRESHOLD

    assert(!(decreases && increases))

    if (!increases && !decreases) {
      None
    } else {
      Some(increases)
    }
  }

}
