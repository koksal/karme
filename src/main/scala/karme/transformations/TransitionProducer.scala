package karme.transformations

import karme.discretization.Discretization
import karme.graphs.StateGraphs.DirectedStateGraph
import karme.graphs.StateGraphs.DiscreteStateGraphNode
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.synthesis.Transitions.Transition

object TransitionProducer {

  /**
    * Produces a set of state transitions weighted by the product of
    * number of measurements for each endpoint.
    */
  def fromDirectedStateGraph(graph: DirectedStateGraph): Set[Transition] = {
    var transitions = Set[Transition]()
    for (edge <- graph.E) {
      for (direction <- graph.edgeDirections(edge)) {
        val source = edge.source(direction)
        val target = edge.target(direction)

        for (label <- graph.edgeLabels(edge)) {
          val weight = source.measurements.size * target.measurements.size
          val sourceState = makeConcreteBooleanState(graph, source)
          val targetState = makeConcreteBooleanState(graph, target)

          // add a transition for the current label
          transitions += Transition(sourceState, targetState.mapping(label),
            label, weight)
        }
      }
    }

    transitions
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