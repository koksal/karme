package karme.transformations

import karme.Experiments.ProbabilisticExperiment
import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.graphs.StateGraphs.StateGraphVertex
import karme.graphs.StateGraphs.UndirectedStateGraphOps
import karme.synthesis.Transitions.Transition
import karme.util.MathUtil

object TransitionProducer {

  private val STABLE_MLE_MARGIN = 0.05

  def positiveTransitions(
    graph: DirectedBooleanStateGraph
  ): Set[Transition] = {
    var transitions = Set[Transition]()
    for (edge <- graph.E) {
      for (direction <- graph.edgeDirections(edge)) {
        val source = graph.source(edge, direction)
        val target = graph.target(edge, direction)

        for (label <- UndirectedStateGraphOps.edgeLabels(edge)) {
          val weight = source.measurements.size * target.measurements.size

          // add a transition for the current label
          transitions += Transition(source.state, target.state.mapping(label),
            label, weight)
        }
      }
    }

    transitions
  }

  // TODO remove stability checks that are handled by three-valued experiment
  // generation
  def negativeTransitions(
    graph: DirectedBooleanStateGraph,
    mleExperiment: ProbabilisticExperiment
  ): Set[Transition] = {
    // for each state, for each stable gene, if there are no neighbors that
    // switch values reliably, produce a "constant" transition.
    var transitions = Set[Transition]()

    for (node <- graph.V) {
      for (label <- mleExperiment.names) {
        if (isStableForLabel(node, label, mleExperiment)) {
          // is the label stable in a neighbor?
          if (!graph.neighbors(node).exists{ neighbor =>
            isStableForLabel(neighbor, label, mleExperiment) &&
              node.state(label) != neighbor.state(label)
          }) {
            // add a self-edge for label
            val weight = node.measurements.size * node.measurements.size
            transitions += Transition(node.state, node.state(label), label,
              weight)
          }
        }
      }
    }

    transitions
  }

  private def isStableForLabel(
    node: StateGraphVertex,
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

}
