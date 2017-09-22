package karme.evaluation.synthetic

import karme.CellTrajectories.CellTrajectory
import karme.Experiments.Experiment
import karme.synthesis.Transitions.ConcreteBooleanState

object SimulationToExperiment {

  def makeExperimentAndTrajectory(
    stateTimestampPairs: Set[(ConcreteBooleanState, Set[Int])]
  ): (Experiment[Boolean], CellTrajectory) = {
    ???
  }

}
