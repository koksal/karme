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
      plotCluster(exp, trajectories, clusterGenes.toSeq,
        s"$name-cluster-$clusterName")
    }
  }

  def plotCluster(
    exp: Experiment[Double],
    trajectories: Seq[CellTrajectory],
    namesToPlot: Seq[String],
    plotName: String
  ): Unit = {
    val avgCurveFolder = reporter.file("cluster-average-curves")
    val allCurvesFolder = reporter.file("cluster-all-curves")

    avgCurveFolder.mkdirs()
    allCurvesFolder.mkdirs()

    for ((t, i) <- trajectories.zipWithIndex) {
      val fname = s"$plotName-curve-$i.pdf"
      plotAverageAndStdev(exp, t, namesToPlot, new File(avgCurveFolder, fname))
      plot(exp, t, namesToPlot, new File(allCurvesFolder, fname))
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
    new ScatterPlot().plot(toPlot, f)
  }

  def plotBooleanCurvesPerGene(
    exp: Experiment[Boolean],
    trajectories: Seq[CellTrajectory],
    folder: File
  ): Unit = {
    def bool2double(b: Boolean): Double = {
      if (b) 1 else 0
    }

    plotCurvesPerGene(exp.mapValues(bool2double), trajectories, folder)

  }

  def plotCurvesPerGene(
    exp: Experiment[Double],
    trajectories: Seq[CellTrajectory],
    folder: File
  ): Unit = {
    folder.mkdirs()

    for ((trajectory, i) <- trajectories.zipWithIndex) {
      for (name <- exp.names) {
        val f = new File(folder, s"trajectory-1-$name.pdf")
        plot(exp, trajectory, Seq(name), f)
      }
    }
  }

  def plot[T: ClassTag](
    exp: Experiment[T],
    trajectory: CellTrajectory,
    names: Seq[String],
    f: File
  ): Unit = {
    new ScatterPlot().plot(labeledPointsForNames(exp, trajectory, names), f)
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
