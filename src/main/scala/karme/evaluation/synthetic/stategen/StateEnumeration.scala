package karme.evaluation.synthetic.stategen

import karme.synthesis.Transitions.ConcreteBooleanState

trait StateEnumeration {

  def enumerateInitialStates(): Seq[Set[ConcreteBooleanState]]

}
