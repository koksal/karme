package karme.transformations

import karme.Experiments.ProbabilisticExperiment
import karme.discretization.Discretization
import karme.graphs.StateGraphs.DirectedStateGraph
import karme.graphs.StateGraphs.DiscreteStateGraphNode
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.synthesis.Transitions.Transition
import karme.util.MathUtil

object TransitionProducer {

  private val STABLE_MLE_MARGIN = 0.1

  /**
    * Produces a set of state transitions weighted by the product of
    * number of measurements for each endpoint.
    */
  def fromDirectedStateGraph(
    graph: DirectedStateGraph,
    mleExperiment: ProbabilisticExperiment
  ): Set[Transition] = {
    var transitions = Set[Transition]()
    for (edge <- graph.E) {
      for (direction <- graph.edgeDirections(edge)) {
        val source = edge.source(direction)
        val target = edge.target(direction)

        for (label <- graph.edgeLabels(edge)) {

          // check that neither state is "ambiguously discretized" for label
          if (isStableForLabel(source, label, mleExperiment) &&
            isStableForLabel(target, label, mleExperiment)) {

            val weight = source.measurements.size * target.measurements.size
            val sourceState = makeConcreteBooleanState(graph, source)
            val targetState = makeConcreteBooleanState(graph, target)

            // add a transition for the current label
            transitions += Transition(sourceState, targetState.mapping(label),
              label, weight)
          }
        }
      }
    }

    transitions
  }

  private def isStableForLabel(
    node: DiscreteStateGraphNode,
    label: String,
    mleExperiment: ProbabilisticExperiment
  ): Boolean = {
    val measurementIDs = node.measurements.map(_.id)
    val nodeMLEMeasurements = measurementIDs map mleExperiment.measurementFromId
    val mleValues = mleExperiment.copy(measurements = nodeMLEMeasurements)
      .valuesForName(label)
    val meanMLE = MathUtil.mean(mleValues)
    math.abs(meanMLE - 0.5) >= STABLE_MLE_MARGIN
  }

  private def makeConcreteBooleanState(
    graph: DirectedStateGraph,
    node: DiscreteStateGraphNode
  ): ConcreteBooleanState = {
    val pairs = graph.names.zip(node.state) map {
      case (name, value) => name -> (value == Discretization.HIGH_VALUE)
    }
    ConcreteBooleanState(pairs.toMap)
  }

}
