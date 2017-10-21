package karme.evaluation.synthetic

import karme.CellTrajectories.CellTrajectory
import karme.Experiments.Experiment
import karme.Experiments.Measurement
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.util.UniqueCounter

import scala.collection.mutable.ListBuffer

object SimulationToExperiment {

  private val counter = new UniqueCounter()

  def makeExperimentAndTrajectory(
    stateTimestampPairs: Set[(ConcreteBooleanState, Seq[Int])]
  ): (Experiment[Boolean], CellTrajectory) = {
    val measurements = ListBuffer[Measurement[Boolean]]()
    var measurementIDtoTime = Map[String, Double]()

    for ((state, timestamps) <- stateTimestampPairs) {
      for (timestamp <- timestamps) {
        val id = freshMeasurementID()
        measurements.append(Measurement(id, state))
        measurementIDtoTime += id -> timestamp
      }
    }

    val experiment = Experiment(measurements.toList)
    (experiment, measurementIDtoTime)
  }

  private def freshMeasurementID(): String = {
    "m" + counter.next
  }

}
