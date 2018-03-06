package karme.evaluation.synthetic.expdesign

import karme.Reporter
import karme.simulation.AsyncBooleanNetworkSimulation
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.util.CollectionUtil

class ExperimentGuideByReachableStates(
  val reporter: Reporter
) extends ExperimentGuide {

  val id = "reachable-states"

  type EvalDomain = Set[ConcreteBooleanState]

  def evaluateModel(
    m: Map[String, FunExpr],
    initialStates: Set[ConcreteBooleanState]
  ): Set[ConcreteBooleanState] = {
    AsyncBooleanNetworkSimulation.simulateOneStep(m, initialStates)
  }

  def distance(
    t1: Set[ConcreteBooleanState],
    t2: Set[ConcreteBooleanState]
  ): Double = {
    println(s"Comparing sets: ${t1.size}, ${t2.size}")
    1 - CollectionUtil.jaccardIndex(t1, t2)
  }
}
