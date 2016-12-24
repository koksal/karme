package karme.synthesis

import karme.Experiments.High
import karme.Experiments.Low
import karme.Experiments.ThreeValued
import karme.Experiments.Uncertain
import karme.synthesis.Trees._

object Transitions {
  abstract class AbstractState[T] {
    def mapping: Map[String, T]
    val orderedKeys: Seq[String] = mapping.keys.toList.sorted
    val orderedValues: Seq[T] = orderedKeys map (k => mapping(k))
    def apply(name: String): T = mapping(name)
    def size: Int = orderedKeys.size
  }

  case class ConcreteBooleanState(
    mapping: Map[String, Boolean]
  ) extends AbstractState[Boolean] {
    override def toString: String = {
      val onKeys = (orderedKeys zip orderedValues) collect {
        case (k, v) if v => k
      }
      onKeys.mkString(", ")
    }
  }

  case class SymBooleanState(
    mapping: Map[String, Variable]
  ) extends AbstractState[Variable] {
    def hasValue(concreteState: ConcreteBooleanState): Expr = {
      val conj = orderedKeys map { key =>
        val symValue = this(key)
        val concValue = concreteState(key)
        Equals(symValue, BooleanLiteral(concValue))
      }
      And(conj: _*)
    }
  }

  case class ThreeValuedState(
    mapping: Map[String, ThreeValued]
  ) extends AbstractState[ThreeValued] {
    override def toString: String = {
      val groupedByValue = mapping.groupBy(_._2)
      val highValues = groupedByValue.getOrElse(High, Set.empty)
      val uncertainValues = groupedByValue.getOrElse(Uncertain, Set.empty)
      val strings = uncertainValues.map(x => x + "?") ++ highValues
      strings.mkString(", ")
    }
  }

  object ThreeValuedState {
    private def orderingIndex(tv: ThreeValued): Int = tv match {
      case Low => 0
      case Uncertain => 1
      case High => 2
    }

    val threeValuedOrdering = Ordering by orderingIndex
  }

  case class Transition(
    input: ConcreteBooleanState,
    output: Boolean,
    label: String,
    weight: Double
  ) {
    private val allLabels = {
      input.orderedKeys
    }

    private def booleanValueString(b: Boolean): String = if (b) "1" else "0"

    val outputString: String = booleanValueString(output)

    override def toString: String = {
      val sb = new StringBuffer()
      sb.append("Input:\n")
      for (l <- this.allLabels) {
        sb.append(s"$l\t= ${if (input(l)) "1" else "0"}\n")
      }
      val outputStr = if (this.output) "1" else "0"
      sb.append("Output:\n")
      sb.append(s"$label = $outputStr")
      sb.toString
    }
  }

}
