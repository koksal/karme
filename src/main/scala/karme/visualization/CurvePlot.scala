package karme.visualization

import java.io.File

import karme.CellTrajectories
import karme.CellTrajectories.CellTrajectory
import karme.Experiments.Experiment
import karme.Reporter
import karme.util.MathUtil

import scala.reflect.ClassTag

/**
  * Plots pseudotime-ordered scatter plots for each dimension (name) in the
  * experiment.
  */
class CurvePlot(implicit reporter: Reporter) {

  def plotClusterCurves(
    exp: Experiment[Double],
    trajectories: Seq[CellTrajectory],
    geneClustering: Map[String, Set[String]],
    name: String
  ): Unit = {
    for ((clusterName, clusterGenes) <- geneClustering) {
      plotAveragesWithBands(exp, trajectories, clusterGenes.toSeq,
        s"$name-cluster-$clusterName")
    }
  }

  def plotAveragesWithBands(
    exp: Experiment[Double],
    trajectories: Seq[CellTrajectory],
    namesToPlot: Seq[String],
    plotName: String
  ): Unit = {
    val curveFolder = reporter.file("cluster-average-curves")
    curveFolder.mkdirs()

    for ((t, i) <- trajectories.zipWithIndex) {
      plotAverageAndStdev(exp, t, namesToPlot,
        new File(curveFolder, s"$plotName-curve-$i.pdf"))
    }
  }

  def plotAverageAndStdev(
    exp: Experiment[Double],
    trajectory: CellTrajectory,
    names: Seq[String],
    f: File
  ): Unit = {
    val labeledPoints = labeledPointsForNames(exp, trajectory, names)
    // group by x, take average, take stdev, produce += stdev
    val pointsGroupedByX = labeledPoints.groupBy(_._1)
    val toPlot = pointsGroupedByX flatMap {
      case (x, xPoints) => {
        val vs = xPoints.map(_._2)
        val avg = MathUtil.mean(vs)
        val std = MathUtil.stdev(vs)
        Seq(
          (x, avg, "mean"),
          (x, avg + std, "+sd"),
          (x, avg - std, "-sd"),
          (x, avg + 2 * std, "+2sd"),
          (x, avg - 2 * std, "-2sd")
        )
      }
    }
    new ScatterPlot(toPlot, f).run()
  }

  def plotBooleanExperiment(
    exp: Experiment[Boolean],
    trajectories: Seq[CellTrajectory],
    folder: File
  ): Unit = {
    folder.mkdirs()

    def bool2int(b: Boolean): Int = {
      if (b) 1 else 0
    }

    val integerValuedExp = exp.mapValues(bool2int)

    for ((trajectory, i) <- trajectories.zipWithIndex) {
      for (name <- integerValuedExp.names) {
        val f = new File(folder, s"trajectory-1-$name.pdf")
        plot(integerValuedExp, trajectory, Seq(name), f)
      }
    }
  }

  def plot[T: ClassTag](
    exp: Experiment[T],
    trajectory: CellTrajectory,
    names: Seq[String],
    f: File
  ): Unit = {
    new ScatterPlot(labeledPointsForNames(exp, trajectory, names), f).run()
  }

  private def labeledPointsForNames[T](
    exp: Experiment[T], trajectory: CellTrajectory, names: Seq[String]
  ): Seq[(Int, T, String)] = {
    assert(names.toSet.subsetOf(exp.names.toSet))

    // order measurements by trajectory
    val trajectoryExp = CellTrajectories.experimentOrderedByTrajectory(exp,
      trajectory)

    names flatMap { name =>
      val values = trajectoryExp.valuesForName(name)
      values.zipWithIndex map {
        case (v, i) => (i, v, name)
      }
    }
  }

}
