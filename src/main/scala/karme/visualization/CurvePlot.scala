package karme.visualization

import java.io.File

import karme.CellTrajectories
import karme.CellTrajectories.CellTrajectory
import karme.Experiments.Experiment
import org.ddahl.rscala.RClient

object CurvePlot {

  /** Plots all components along the trajectory. */
  def plot[T](
    exp: Experiment[T], trajectory: CellTrajectory, outFolder: File
  ) : Unit = {
    val R = RClient()
    R eval "library(ggplot2)"

    // order measurements by trajectory
    val orderedIDs = CellTrajectories.cellOrder(trajectory)
    val orderedMs = orderedIDs map exp.measurementFromId
    val trajectoryExp = exp.copy(measurements = orderedMs)

    val folder = new File(outFolder, "curves")
    folder.mkdirs()

    // plot one chart per name
    for (name <- exp.names) {
      val valueArray = trajectoryExp.valuesForName(name).map(_.toString).toArray
      val indexArray = valueArray.indices.toArray

      R.set("indices", indexArray)
      R.set("values", valueArray)

      R.eval("data <- data.frame(index = indices, value = values)")

      R.eval("plot = ggplot(data, aes(x = index, y = value)) + geom_line()")

      val f = new File(folder, s"$name.pdf")

      R.set("fname", f.getAbsolutePath())
      R.eval("ggsave(plot, file = fname)")
    }
  }

}
