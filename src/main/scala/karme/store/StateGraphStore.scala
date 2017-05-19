package karme.store

import java.io.File

import karme.graphs.StateGraphs.DirectedBooleanStateGraph

class StateGraphStore(parent: File) {

  val storeLocation = "state-graph.csv"

  def store(g: DirectedBooleanStateGraph): Unit = {
    // TODO store edges
    // store nodes to measurement ids
  }

  def read(): DirectedBooleanStateGraph = {
    ???
  }

}
