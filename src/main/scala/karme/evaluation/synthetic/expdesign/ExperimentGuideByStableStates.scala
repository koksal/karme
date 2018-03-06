package karme.evaluation.synthetic.expdesign

import karme.Reporter
import karme.evaluation.synthetic.FixpointStates
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.util.CollectionUtil

class ExperimentGuideByStableStates(
  val reporter: Reporter
) extends ExperimentGuide {

  val id = "stable-states"

  type EvalDomain = Set[ConcreteBooleanState]

  def evaluateModel(
    m: Map[String, FunExpr],
    initialStates: Set[ConcreteBooleanState]
  ): Set[ConcreteBooleanState] = {
    FixpointStates.findSimulationFixpoints(m, initialStates)
  }

  def distance(
    t1: Set[ConcreteBooleanState],
    t2: Set[ConcreteBooleanState]
  ): Double = {
    1 - CollectionUtil.jaccardIndex(t1, t2)
  }

}
