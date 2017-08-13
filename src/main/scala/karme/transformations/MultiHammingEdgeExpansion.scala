package karme.transformations

import karme.graphs.Graphs.EdgeDirection
import karme.graphs.Graphs.UnlabeledEdge
import karme.graphs.StateGraphs
import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.graphs.StateGraphs.StateGraphVertex
import karme.graphs.StateGraphs.UndirectedStateGraphOps
import karme.synthesis.Transitions.ConcreteBooleanState

import scala.collection.mutable.ListBuffer

class MultiHammingEdgeExpansion(g: DirectedBooleanStateGraph) {

  def expandMultiHammingEdges(): DirectedBooleanStateGraph = {
    // for each order
    //   for each intermediary state along the order
    //     add "empty" state if it doesn't exist
    //     add 1-Hamming link from the previous node

    var newG = g

    for {
      e <- g.E
      dir <- g.edgeDirections(e)
    } {
      val labels = UndirectedStateGraphOps.edgeLabels(e)

      if (labels.size > 1) {
        newG = newG.removeEdge(e)

        val seqs = allStateSequences(labels, g.source(e, dir), g.target(e, dir))

        for (seq <- seqs) {
          newG = addSequence(newG, seq)
        }
      }
    }

    newG
  }

  private def addSequence(
    g: DirectedBooleanStateGraph, stateSeq: Seq[ConcreteBooleanState]
  ): DirectedBooleanStateGraph = {
    val adjacentStates = stateSeq.zip(stateSeq.tail)

    var newG = g
    for ((s1, s2) <- adjacentStates) {
      val sourceNode = newG.V.find(_.state == s1).get

      newG.V.find(_.state == s2) match {
        case Some(targetNode) => {
          newG = newG.addEdge(sourceNode, targetNode)
        }
        case None => {
          val newNode = StateGraphs.makeNode(s2, Nil)
          newG = newG.addVertex(newNode)
          newG = newG.addEdge(sourceNode, newNode)
        }
      }
    }

    newG
  }

  private def allStateSequences(
    differingNames: Seq[String],
    source: StateGraphVertex,
    target: StateGraphVertex
  ): Seq[Seq[ConcreteBooleanState]] = {
    // gather before/after values for each label
    val nameOrderings = allNameOrderings(differingNames)

    // apply all serial combinations of value change
    nameOrderings map (
      ordering => stateSequenceForSwitchOrder(ordering, source))
  }

  private def stateSequenceForSwitchOrder(
    switchOrder: Seq[String],
    source: StateGraphVertex
  ): Seq[ConcreteBooleanState] = {
    val states = ListBuffer(source.state)

    for (switchingName <- switchOrder) {
      val lastState = states.last
      val newState =
        lastState.replaceValue(switchingName, !lastState.value(switchingName))

      states.append(newState)
    }

    states.toList
  }

  private def allNameOrderings(names: Seq[String]): Seq[Seq[String]] = {
    if (names.isEmpty) {
      Seq(Seq())
    } else {
      (0 until names.size).flatMap { i =>
        val firstElem = names(i)
        val restElems = names.take(i) ++ names.drop(i + 1)
        val restOrderings = allNameOrderings(restElems)
        restOrderings map {
          o => firstElem +: o
        }
      }
    }
  }

}
