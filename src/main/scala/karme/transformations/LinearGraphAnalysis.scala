package karme.transformations

import karme.{CellTrajectories, PredictionLibrary}
import karme.CellTrajectories.CellTrajectory
import karme.Experiments.Experiment
import karme.synthesis.Transitions.ConcreteBooleanState

object LinearGraphAnalysis {

  val distComp = new RankSumTest()
  val pValue = 0.01

  def analyze(
    experiment: Experiment[Double],
    trajectory: CellTrajectory,
    kdExp: PredictionLibrary
  ): Unit = {
    val orderedMs = CellTrajectories.orderMeasurementsByTrajectory(experiment,
      trajectory).measurements
    val cellTree = HierarchicalCellTrees.buildCellHierarchy(orderedMs)

    val levels = 1 to 3

    val levelToBooleanStates = levels.map{ l =>
      l -> makeBooleanStatesForLevel(cellTree, l)
    }.toMap

    checkDistinctStates(experiment.names, levels, levelToBooleanStates)
    checkAgainstKD(experiment.names, levels, levelToBooleanStates, kdExp)
  }

  def checkAgainstKD(
    names: Seq[String],
    levels: Seq[Int],
    levelToStates: Map[Int, Seq[ConcreteBooleanState]],
    kdExp: PredictionLibrary
  ): Unit = {
    for (p <- kdExp.predictions.sortBy(p => - math.abs(p.weight))) {
      if (!names.contains(p.source) || !names.contains(p.target)) {
        val row = List(
          p.source,
          p.target,
          p.weight,
          "Missing data"
        )
        println(row.mkString("\t"))
      } else {
        for (level <- levels) {
          val srcValues = levelToStates(level).map(s => s.value(p.source))
          val tgtValues = levelToStates(level).map(s => s.value(p.target))

          val sourcePrecedes =
            firstChangeIndex(srcValues) <= firstChangeIndex(tgtValues) &&
              firstChangeIndex(srcValues) >= 0 &&
              firstChangeIndex(tgtValues) >= 0

          val row = List(
            p.source,
            p.target,
            p.weight,
            level,
            valueString(srcValues),
            valueString(tgtValues),
            firstChangeIndex(srcValues),
            firstChangeIndex(tgtValues),
            sourcePrecedes
          )
          println(row.mkString("\t"))
        }
      }
    }
  }

  def firstChangeIndex(vs: Seq[Boolean]): Int = {
    vs.zip(vs.tail).indexWhere {
      case (v1, v2) => {
        v1 != v2
      }
    }
  }

  def checkDistinctStates(
    names: Seq[String],
    levels: Seq[Int],
    levelToStates: Map[Int, Seq[ConcreteBooleanState]]
  ): Unit = {
    for (i <- levels) {
      val valuesPerName = for (name <- names) yield {
        levelToStates(i).map(s => s.value(name))
      }

      val distinctValues = valuesPerName.distinct
      println(s"Level $i:")
      println(s"All values: ${valuesPerName.size}")
      println(s"Distinct values: ${distinctValues.size}")
    }
  }

  def makeBooleanStatesForLevel(
    cellTree: HierarchicalCellTree,
    i: Int
  ): Seq[ConcreteBooleanState] = {
    val groups = HierarchicalCellTrees.findMeasurementSetsAtLevel(cellTree, i)
    val discretization = new PairwiseComparisonDiscretization(distComp,
      pValue)

    discretization.makeBooleanStatesPerGroup(groups)
  }

  def valueString(vs: Seq[Boolean]): String = {
    vs.map(v => if (v) "1" else "O").mkString("")
  }

}
