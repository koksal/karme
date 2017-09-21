package karme.evaluation.synthetic.stategen

import karme.synthesis.Transitions.ConcreteBooleanState
import karme.synthesis.Transitions.GenericState

import scala.util.Random

class RandomStateGeneration(variables: Set[String]) {

  private val random = new Random()

  def generate(): ConcreteBooleanState = {
    val mapping = variables.map(v => v -> random.nextBoolean()).toMap
    GenericState(mapping)
  }

}
