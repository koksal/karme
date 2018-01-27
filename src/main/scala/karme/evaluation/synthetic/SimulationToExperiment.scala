package karme.evaluation.synthetic

import karme.CellTrajectories.CellTrajectory
import karme.Experiments
import karme.Experiments.{BooleanExperiment, BooleanMeasurement, Experiment}
import karme.evaluation.synthetic.examples.myeloid.MyeloidModel
import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.synthesis.Transitions.ConcreteBooleanState

import scala.util.Random

class SimulationToExperiment(random: Random)(
  temporalNoiseSigma: Double,
  measurementNoiseProbability: Double,
  measurementDropProbability: Double
) {

  val NB_OBS_PER_STATE = 100

  val protectedStates =
    MyeloidModel.stableStates() + MyeloidModel.makeInitialState()

  val temporalNoise = new TemporalNoise(random)(temporalNoiseSigma)
  val measurementNoise = new MeasurementNoise(random)(measurementNoiseProbability)

  def generateExperiment(
    baseStateGraph: DirectedBooleanStateGraph,
    baseTrajectory: CellTrajectory
  ): (BooleanExperiment, CellTrajectory) = {
    // generate measurements and times from each timed boolean state

    var measurements = Set[BooleanMeasurement]()
    var trajectory = Map[String, Double]()

    for (v <- baseStateGraph.V) {
      val vTimes = v.measurements.map(m => baseTrajectory(m.id))
      assert(vTimes.toSet.size == 1)

      val (newMs, newTraj) = generateObservationsForState(v.state, vTimes.head)
      measurements ++= newMs
      trajectory ++= newTraj
    }

    (Experiment(measurements.toSeq), trajectory)
  }

  def generateObservationsForState(
    baseState: ConcreteBooleanState,
    baseTime: Double
  ): (Set[BooleanMeasurement], CellTrajectory) = {
    var observations = Set[BooleanMeasurement]()
    var trajectory = Map[String, Double]()

    val stateIsProtected = protectedStates.contains(baseState)

    for (i <- 1 to NB_OBS_PER_STATE) {
      val time = temporalNoise.addNoise(baseTime)
      val measuredState = if (stateIsProtected) {
        baseState
      } else {
        measurementNoise.addNoise(baseState)
      }

      if (stateIsProtected ||
        random.nextDouble() >= measurementDropProbability) {
        val measurement = Experiments.makeMeasurement(measuredState)
        observations += measurement
        trajectory += measurement.id -> time
      }
    }

    (observations, trajectory)
  }

}
