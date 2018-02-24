package karme.evaluation.synthetic

import karme.CellTrajectories.CellTrajectory
import karme.Experiments
import karme.Experiments.{BooleanExperiment, BooleanMeasurement, Experiment}
import karme.evaluation.synthetic.examples.myeloid.MyeloidModel
import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.util.CollectionUtil

import scala.util.Random

class SimulationToExperiment(random: Random)(
  temporalNoiseSigma: Double,
  stateFalseDiscoveryRate: Double,
  stateTruePositiveRate: Double
) {

  val NB_OBS_PER_STATE = 10

  val protectedStates =
    MyeloidModel.stableStates + MyeloidModel.makeInitialState()

  val temporalNoise = new TemporalNoise(random)(temporalNoiseSigma)
  val measurementNoise = new MeasurementNoise(random)(0.01)

  def generateExperiment(
    baseStateGraph: DirectedBooleanStateGraph,
    baseTrajectory: CellTrajectory
  ): (BooleanExperiment, CellTrajectory) = {
    val baseStates = baseStateGraph.V.map(_.state)

    var measurements = Set[BooleanMeasurement]()
    var trajectory = Map[String, Double]()

    for (v <- baseStateGraph.V) {
      val vTimes = v.measurements.map(m => baseTrajectory(m.id))
      assert(vTimes.toSet.size == 1)

      val (newMs, newTraj) = generateObservationsForState(v.state, vTimes.head)
      measurements ++= newMs
      trajectory ++= newTraj
    }

    // remove measurements as long as the state is not protected
    while (tpr(measurements, baseStates) > stateTruePositiveRate) {
      val randMeasurement = CollectionUtil.randomElement(random)(measurements)

      if (!protectedStates.contains(randMeasurement.state)) {
        measurements -= randMeasurement
        trajectory -= randMeasurement.id
      }
    }

    // add measurements that add new states until FDR is reached
    while (fdr(measurements, baseStates) < stateFalseDiscoveryRate) {
      val randMeasurement = CollectionUtil.randomElement(random)(measurements)

      if (!protectedStates.contains(randMeasurement.state)) {
        val noisyState = measurementNoise.addNoise(randMeasurement.state)

        if (!baseStates.contains(noisyState)) {
          val noisyMeasurement = Experiments.makeMeasurement(noisyState)
          measurements += noisyMeasurement
          trajectory += noisyMeasurement.id -> trajectory(randMeasurement.id)
        }
      }
    }

    (Experiment(measurements.toSeq), trajectory)
  }

  def tpr(
    measurements: Set[BooleanMeasurement],
    baseStates: Set[ConcreteBooleanState]
  ): Double = {
    val measurementStates = measurements.map(_.state)
    val tp = measurementStates.intersect(baseStates)
    tp.size.toDouble / baseStates.size.toDouble
  }

  def fdr(
    measurements: Set[BooleanMeasurement],
    baseStates: Set[ConcreteBooleanState]
  ): Double = {
    val measurementStates = measurements.map(_.state)
    val fp = measurementStates.diff(baseStates)
    fp.size.toDouble / measurementStates.size.toDouble
  }

  def generateObservationsForState(
    baseState: ConcreteBooleanState,
    baseTime: Double
  ): (Set[BooleanMeasurement], CellTrajectory) = {
    var observations = Set[BooleanMeasurement]()
    var trajectory = Map[String, Double]()

    for (i <- 1 to NB_OBS_PER_STATE) {
      val time = temporalNoise.addNoise(baseTime)
      val measurement = Experiments.makeMeasurement(baseState)
      observations += measurement
      trajectory += measurement.id -> time
    }

    (observations, trajectory)
  }

  def generateObservationsForStateOld(
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
        random.nextDouble() >= stateTruePositiveRate) {
        val measurement = Experiments.makeMeasurement(measuredState)
        observations += measurement
        trajectory += measurement.id -> time
      }
    }

    (observations, trajectory)
  }

}
