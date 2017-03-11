package karme

import karme.discretization.MclustInterface
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
    e: ProbabilisticExperiment,
    uncertaintyMargin: Double
  ): ThreeValuedExperiment = {
    val MID_VALUE = 0.5

    val threeValuedMeasurements = e.measurements map { m =>
      val threeValState = m.state.mapValues[ThreeValued] { v =>
        assert(v >= 0 && v <= 1)
        if (math.abs(MID_VALUE - v) <= uncertaintyMargin) {
          Uncertain
        } else if (v < MID_VALUE) {
          Low
        } else {
          High
        }
      }
      m.copy(state = threeValState)
    }
    e.copy(measurements = threeValuedMeasurements)
  }

  def threeValuedExpFromMixtureModel(
    e: ProbabilisticExperiment,
    uncertaintyThreshold: Double
  ): ThreeValuedExperiment = {
    var nameToThreeValuedSeq = Map[String, Seq[ThreeValued]]()

    for (name <- e.names) {
      val vs = e.valuesForName(name)
      val mclustRes = MclustInterface.run(vs, 1, 2)

      if (mclustRes.g != 2) {
        println(s"There are ${mclustRes.g} optimal components for ${name}.")
      } else {
        val threeValuedSeq =
          mclustRes.classification.zip(mclustRes.uncertainty) map {
            case (clss, uncertainty) => {
              if (uncertainty > uncertaintyThreshold) {
                Uncertain
              } else if (clss == 1) {
                Low
              } else if (clss == 2) {
                High
              } else {
                sys.error(s"Unexpected class: ${clss}")
              }
            }
          }
        nameToThreeValuedSeq += name -> threeValuedSeq
      }
    }

    val namesWithTwoComponents = nameToThreeValuedSeq.keySet.toSeq.sorted
    val threeValuedMatrix = namesWithTwoComponents map (
      n => nameToThreeValuedSeq(n))
    val threeValuedPerMeasurement = threeValuedMatrix.transpose

    val threeValuedMs = e.measurements.zip(threeValuedPerMeasurement) map {
      case (m, vs) => {
        m.copy(state = GenericState(namesWithTwoComponents.zip(vs).toMap))
      }
    }

    e.copy(measurements = threeValuedMs)
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
