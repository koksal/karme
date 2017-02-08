package karme

import karme.synthesis.Transitions.GenericState
import karme.synthesis.Transitions.ConcreteBooleanState

import scala.collection.mutable

object Experiments {

  case class Measurement[T](id: String, state: GenericState[T])

  case class Experiment[T](
    measurements: Seq[Measurement[T]]
  ) {

    assert(repOk())

    private def repOk(): Boolean = {
      measurements.forall(m => m.state.orderedKeys == this.names)
    }

    private lazy val idToMeasurement: Map[String, Measurement[T]] =
      measurements.map{ m =>
        m.id -> m
      }.toMap

    def names: Seq[String] = {
      measurements.head.state.orderedKeys
    }

    def valuesForName(name: String): Seq[T] = {
      measurements map (m => m.state.value(name))
    }

    def partitionClusters(
      clustering: mutable.MultiMap[String, String]
    ): Map[String, Experiment[T]] = {
      val idToMeasurement = measurements.map(m => m.id -> m).toMap

      val pairs = for ((cluster, ids) <- clustering) yield {
        val sortedIds = ids.toList.sorted
        val clusterMs = sortedIds map (id => idToMeasurement(id))
        cluster -> new Experiment(clusterMs)
      }
      pairs.toMap
    }

    def project(names: Set[String]): Experiment[T] = {
      val projectedMeasurements = measurements map { m =>
        assert(names.subsetOf(m.state.orderedKeys.toSet))
        val projectedMapping = names.map(n => n -> m.state.value(n)).toMap
        val projectedState = new GenericState[T](projectedMapping)
        m.copy[T](state = projectedState)
      }
      Experiment[T](projectedMeasurements)
    }

    def measurementFromId(id: String): Measurement[T] = idToMeasurement(id)
  }

  def probabilisticExperimentToThreeValued(
    e: ProbabilisticExperiment
  ): ThreeValuedExperiment = {
    // TODO take this as argument
    val UNCERTAINTY_MARGIN = 0.05
    val MID_VALUE = 0.5

    val threeValuedMeasurements = e.measurements map { m =>
      val threeValMapping = m.state.mapping map { case(k, v) =>
        assert(v >= 0 && v <= 1)
        val tv: ThreeValued = if (math.abs(MID_VALUE - v) <= UNCERTAINTY_MARGIN) {
          Uncertain
        } else if (v < MID_VALUE) {
          Low
        } else {
          High
        }
        k -> tv
      }
      m.copy(state = new GenericState[ThreeValued](threeValMapping))
    }
    e.copy(measurements = threeValuedMeasurements)
  }

  def booleanStatesToExperiment(
    ss: Set[ConcreteBooleanState]
  ): BooleanExperiment = {
    assert(ss.nonEmpty)
    val measurements = ss.toSeq.zipWithIndex map {
      case (s, i) =>
        val id = s"m_$i"
        Measurement(id, s)
    }
    Experiment(measurements)
  }

  def threeValuedToBooleanSet(v: ThreeValued): Set[Boolean] = v match {
    case Low => Set(false)
    case High => Set(true)
    case Uncertain => Set(false, true)
  }

  sealed trait ThreeValued
  case object Low extends ThreeValued
  case object Uncertain extends ThreeValued
  case object High extends ThreeValued

  type ContinuousExperiment = Experiment[Double]
  type ContinuousMeasurement = Measurement[Double]

  type DiscreteExperiment = Experiment[Int]
  type DiscreteMeasurement = Measurement[Int]

  type BooleanExperiment = Experiment[Boolean]
  type BooleanMeasurement = Measurement[Boolean]

  type ProbabilisticExperiment = Experiment[Double]
  type ProbabilisticMeasurement = Measurement[Double]

  type ThreeValuedExperiment = Experiment[ThreeValued]
  type ThreeValuedMeasurement = Measurement[ThreeValued]

}
