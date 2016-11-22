package karme.synthesis

import karme.synthesis.Trees._
import karme.util.MathUtil

object Transitions {
  class AbsBooleanState[T](mapping: Map[String, T]) {
    val orderedKeys: Seq[String] = mapping.keys.toList.sorted
    val orderedValues: Seq[T] = orderedKeys map (k => mapping(k))
    def apply(name: String): T = mapping(name)
  }

  case class ProbabilisticBoolean(value: Boolean, posterior: Double)

  case class ConcreteBooleanState(
    mapping: Map[String, Boolean]
  ) extends AbsBooleanState(mapping) {
    override def toString: String = {
      val onKeys = (orderedKeys zip orderedValues) collect {
        case (k, v) if v => k
      }
      onKeys.mkString(", ")
    }
  }

  case class ConcreteProbabilisticBooleanState(
    mapping: Map[String, ProbabilisticBoolean]
  ) extends AbsBooleanState(mapping) {
    override def toString: String = {
      val keyPostPairs = (orderedKeys zip orderedValues) collect {
        case (k, v) if v.value => k -> v.posterior
      }
      keyPostPairs.map{ case (k, p) => s"$k (${MathUtil.roundTo(p, 2)})" }
        .mkString(", ")
    }
    def toConcreteBooleanState: ConcreteBooleanState = {
      val m = this.mapping map {
        case (k, pv) => k -> pv.value
      }
      ConcreteBooleanState(m)
    }
  }

  case class Transition(
    s1: ConcreteBooleanState,
    s2: ConcreteBooleanState,
    weight: Double
  ) {
    private val allLabels = {
      assert(s1.orderedKeys == s2.orderedKeys)
      s1.orderedKeys
    }

    val diffIndex: Int = {
      (s1.orderedValues zip s2.orderedValues) indexWhere {
        case (b1, b2) => b1 != b2
      }
    }

    val label: String = this.allLabels(this.diffIndex)

    val output: Boolean = s2(this.label)

    override def toString: String = {
      val sb = new StringBuffer()
      sb.append("Input:\n")
      for (l <- this.allLabels) {
        sb.append(s"$l\t= ${if (s1(l)) "1" else "0"}\n")
      }
      val outputStr = if (this.output) "1" else "0"
      sb.append("Output:\n")
      sb.append(s"$label = $outputStr")
      sb.toString
    }
  }

  case class SymBooleanState(
    mapping: Map[String, Variable]
  ) extends AbsBooleanState(mapping) {
    def hasValue(concreteState: ConcreteBooleanState): Expr = {
      val conj = orderedKeys map { key =>
        val symValue = this(key)
        val concValue = concreteState(key)
        Equals(symValue, BooleanLiteral(concValue))
      }
      And(conj: _*)
    }
  }

}
