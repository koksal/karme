package karme.transformations

import karme.CellTrajectories
import karme.CellTrajectories.CellTrajectory
import karme.Experiments.Experiment

object LinearGraphAnalysis {

  def analyze(
    experiment: Experiment[Double],
    trajectory: CellTrajectory
  ) = {
    val orderedMs = CellTrajectories.orderMeasurementsByTrajectory(experiment,
      trajectory).measurements
    val cellTree = HierarchicalCellTrees.buildCellHierarchy(orderedMs)

    val distComp = new RankSumTest()
    val pValue = 0.01

    for (i <- 1 to 3) {
      val groups = HierarchicalCellTrees.findMeasurementSetsAtLevel(cellTree, i)
      val discretization = new PairwiseComparisonDiscretization(distComp,
        pValue)

      val booleanStates = discretization.makeBooleanStatesPerGroup(groups)

      val valuesPerName = for (name <- experiment.names) yield {
        booleanStates.map(s => s.value(name))
      }

      val distinctValues = valuesPerName.distinct
      println(s"Level $i:")
      println(s"Distinct values: ${distinctValues.size}")

      for ((name, values) <- experiment.names.zip(valuesPerName)) {
        println(name)
        val valueStr = values.map(v => if (v) "1" else "0").mkString("")
        println(valueStr)
      }
    }

  }

}
