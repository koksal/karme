package karme.visualization

import java.io.File

import karme.CellTrajectories
import karme.CellTrajectories.CellTrajectory
import karme.Experiments.Experiment
import org.ddahl.rscala.RClient

/**
  * Plots pseudotime-ordered scatter plots for each dimension (name) in the
  * experiment.
  */
object CurvePlot {

  def plot[T](
    exp: Experiment[T], trajectory: CellTrajectory, outFolder: File
  ) : Unit = {
    // order measurements by trajectory
    val orderedIDs = CellTrajectories.cellOrder(trajectory)
    val orderedMs = orderedIDs map exp.measurementFromId
    val trajectoryExp = exp.copy(measurements = orderedMs)

    outFolder.mkdirs()

    // plot one scatter plot per name
    for (name <- exp.names) {
      val valueArray = trajectoryExp.valuesForName(name).map(_.toString).toArray
      val indexArray = valueArray.indices.toArray

      val f = new File(outFolder, s"$name.pdf")
      ScatterPlot.plot(indexArray, valueArray, f)
    }
  }

}
