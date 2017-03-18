package karme.synthesis

import karme.Experiments.High
import karme.Experiments.Low
import karme.Experiments.ThreeValued
import karme.Experiments.Uncertain
import karme.synthesis.Trees._

object Transitions {
  case class GenericState[T](mapping: Map[String, T]) {
    val orderedKeys: Seq[String] = mapping.keys.toList.sorted
    val orderedValues: Seq[T] = orderedKeys map (k => mapping(k))
    def value(name: String): T = mapping(name)
    def size: Int = orderedKeys.size
    def mapValues[U](f: T => U): GenericState[U] = {
      val newMapping = mapping map {
        case (k, v) => k -> f(v)
      }
      GenericState(newMapping)
    }
  }

  type ConcreteBooleanState = GenericState[Boolean]
  type SymBooleanState = GenericState[Variable]
  type ThreeValuedState = GenericState[ThreeValued]

  def printConcreteBooleanState(s: ConcreteBooleanState): String = {
    val onKeys = (s.orderedKeys zip s.orderedValues) collect {
      case (k, v) if v => k
    }
    onKeys.mkString(", ")
  }

  // TODO adapt to scala 2.12
  def printThreeValuedState(s: ThreeValuedState): String = {
    // val groupedByValue = s.mapping.groupBy(_._2)
    // val highValues = groupedByValue.getOrElse(High, Set.empty)
    // val uncertainValues = groupedByValue.getOrElse(Uncertain, Set.empty)
    // val strings = uncertainValues.map(x => x + "?") ++ highValues
    // strings.mkString(", ")
    "TODO"
  }

  object ThreeValuedState {
    private def orderingIndex(tv: ThreeValued): Int = tv match {
      case Low => 0
      case Uncertain => 1
      case High => 2
    }

    val threeValuedOrdering = Ordering by orderingIndex
  }

  import scala.language.existentials

  case class Transition(
    input: GenericState[Boolean],
    output: Boolean,
    label: String,
    weight: Double
  ) {
    val outputString: String = if (output) label else s"!$label"

    override def toString: String = {
      printConcreteBooleanState(input) + " -> " + outputString
    }
  }

}
