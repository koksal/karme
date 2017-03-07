package karme.visualization

import java.io.File

import karme.CellTrajectories
import karme.CellTrajectories.CellTrajectory
import karme.Experiments.Experiment
import karme.util.MathUtil

import scala.reflect.ClassTag

/**
  * Plots pseudotime-ordered scatter plots for each dimension (name) in the
  * experiment.
  */
object CurvePlot {

  def plotClusterGenes(
    exp: Experiment[Double],
    trajectories: Seq[CellTrajectory],
    geneClustering: Map[Int, Set[String]],
    outFolder: File
  ): Unit = {
    for ((clusterIndex, clusterGenes) <- geneClustering) {
      plotAllTrajectories(exp, trajectories, clusterGenes.toSeq,
        s"cluster-${clusterIndex}-genes", outFolder)
    }
  }

  def plotAllTrajectories(
    exp: Experiment[Double],
    trajectories: Seq[CellTrajectory],
    namesToPlot: Seq[String],
    plotName: String,
    outFolder: File
  ): Unit = {
    val curveFolder = new File(outFolder, "curves")
    curveFolder.mkdirs()

    for ((t, i) <- trajectories.zipWithIndex) {
      CurvePlot.plotAverageAndStdev(exp, t, namesToPlot,
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
          (x, avg + 2 * std, "+2*sd"),
          (x, avg - 2 * std, "-2*sd")
        )
      }
    }
    ScatterPlot.plot(toPlot, f)
  }

  def plot[T: ClassTag](
    exp: Experiment[T],
    trajectory: CellTrajectory,
    names: Seq[String],
    f: File
  ) : Unit = {
    ScatterPlot.plot(labeledPointsForNames(exp, trajectory, names), f)
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
