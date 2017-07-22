package karme.transformations

import karme.graphs.StateGraphs
import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.graphs.StateGraphs.StateGraphVertex
import karme.graphs.StateGraphs.UndirectedStateGraphOps
import karme.synthesis.Transitions.Transition

object TransitionProducer {

  def producePositiveAndNegativeTransitions(
    directedStateGraph: DirectedBooleanStateGraph
  ): (Set[Transition], Set[Transition]) = {
    val stateNames = StateGraphs.namesFromStateGraph(directedStateGraph)

    val positiveTransitions = TransitionProducer.positiveTransitions(
      directedStateGraph)

    val negativeTransitions = TransitionProducer.negativeTransitions(
      directedStateGraph, stateNames)

    (positiveTransitions, negativeTransitions)
  }

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

  def negativeTransitions(
    graph: DirectedBooleanStateGraph,
    labels: Iterable[String]
  ): Set[Transition] = {
    var transitions = Set[Transition]()

    for (node <- graph.V) {
      for (label <- labels) {
        // if there is no neighbor with a different value for the label, add
        // a self-edge for the label
        if (!graph.targets(node).exists{ neighbor =>
          node.state.value(label) != neighbor.state.value(label)
        }) {
          val weight = node.measurements.size * node.measurements.size
          transitions += Transition(node.state, node.state.value(label), label,
            weight)
        }
      }
    }

    transitions
  }

}
