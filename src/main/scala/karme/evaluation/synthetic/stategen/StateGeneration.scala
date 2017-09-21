package karme.evaluation.synthetic.stategen

import karme.synthesis.Transitions.ConcreteBooleanState

trait StateGeneration {

  def generateInitialStates(): Set[ConcreteBooleanState]

}
