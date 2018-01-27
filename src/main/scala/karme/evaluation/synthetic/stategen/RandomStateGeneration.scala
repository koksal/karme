package karme.evaluation.synthetic.stategen

import karme.synthesis.Transitions.{ConcreteBooleanState, GenericState}

import scala.util.Random

class RandomStateGeneration(variables: Set[String]) extends StateGeneration {

  private val random = new Random()

  def generateInitialStates(): Set[ConcreteBooleanState] = {
    val mapping = variables.map(v => v -> random.nextBoolean()).toMap
    Set(GenericState(mapping))
  }

}
