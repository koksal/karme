package karme.synthesis

import karme.Experiments.{High, Low, ThreeValued, Uncertain}
import karme.synthesis.Trees._

object Transitions {
  case class GenericState[T](mapping: Map[String, T]) {
    val orderedKeys: Seq[String] = mapping.keys.toList.sorted
    val orderedValues: Seq[T] = orderedKeys map (k => mapping(k))
    def value(name: String): T = mapping(name)
    def size: Int = orderedKeys.size
    def mapKeys(f: String => String): GenericState[T] = {
      val newMapping = mapping map {
        case (k, v) => f(k) -> v
      }
      GenericState(newMapping)
    }
    def mapValues[U](f: T => U): GenericState[U] = {
      val newMapping = mapping map {
        case (k, v) => k -> f(v)
      }
      GenericState(newMapping)
    }
    def replaceValue(name: String, newValue: T): GenericState[T] = {
      this.copy(mapping = this.mapping + (name -> newValue))
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
