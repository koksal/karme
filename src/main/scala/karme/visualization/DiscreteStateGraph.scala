package karme.visualization

import java.io.File

import karme.Experiments.DiscreteExperiment

import scala.collection.mutable

object DiscreteStateGraph {

  def plot(
    exp: DiscreteExperiment,
    clustering: mutable.MultiMap[String, String],
    outFile: File
  ): Unit = {
    // create dot string
    // write it to temp file
    // invoke dot with it
    // delete temp file
  }

  private def dotString(
    exp: DiscreteExperiment, clustering: mutable.MultiMap[String, String]
  ): String = {
    // find out unique states
    // for each state, find out:
    //   - nb cells
    //   - which clusters have the cell
    "digraph G {\n" +
      dotNodes(exp) + "\n" +
      dotEdges(exp) + "\n" +
    "}"
  }

  private def dotNodes(exp: DiscreteExperiment): String = {

  }

  private def dotEdges(exp: DiscreteExperiment): String = {

  }

}
