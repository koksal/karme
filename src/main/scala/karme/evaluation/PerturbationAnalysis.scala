package karme.evaluation

import karme.Clustering
import karme.Reporter
import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.simulation.AsyncBooleanNetworkSimulation
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.visualization.StateGraphPlotter

/**
  * Perturbs initial states and assesses reachability of genes of interest.
  */
class PerturbationAnalysis(
  labelToFunction: Map[String, FunExpr],
  graph: DirectedBooleanStateGraph,
  clustering: Clustering,
  namesToObserve: Set[String]
)(implicit reporter: Reporter) {

  private val stateGraphPlotter = new StateGraphPlotter(reporter)

  def findGeneDrivers(
    initialStates: Set[ConcreteBooleanState]
  ) = {
    val originalRatios = computeExpressedStateRatios(initialStates,
      "simulation-original")

    for {
      (perturbedName, perturbedStateSets) <- perturbStates(initialStates)
      perturbedStates <- perturbedStateSets
    } {
      val perturbedValue = perturbedStates.head.value(perturbedName)
      println(s"Perturbing $perturbedName.")
      println(s"Perturbed value: $perturbedValue")

      val perturbedRatios = computeExpressedStateRatios(perturbedStates,
        s"simulation-$perturbedName-$perturbedValue")
      val ratioDifferences = computeRatioDifferences(originalRatios,
        perturbedRatios)

      for ((observedName, diff) <- ratioDifferences.toList.sortBy(- _._2)) {
        println(s"Expression ratio difference for $observedName: $diff")
      }
    }
  }

  def computeRatioDifferences(
    originalRatios: Map[String, Double],
    perturbedRatios: Map[String, Double]
  ): Map[String, Double] = {
    originalRatios map {
      case (name, origRatio) => {
        name -> math.abs(origRatio - perturbedRatios(name))
      }
    }
  }

  def computeExpressedStateRatios(
    initialStates: Set[ConcreteBooleanState],
    plotLabel: String
  ): Map[String, Double] = {
    val reachedStates = AsyncBooleanNetworkSimulation.simulate(labelToFunction,
      initialStates)

    stateGraphPlotter.plotDirectedGraph(graph, plotLabel,
      nodeHighlightGroups = List(reachedStates))

    val nameToRatio = for (name <- initialStates.head.orderedKeys) yield {
      val nbExpressed = reachedStates.count(s => s.value(name))
      val nbNonExpressed = reachedStates.count(s => !s.value(name))
      assert(nbExpressed + nbNonExpressed == reachedStates.size)

      name -> (nbExpressed.toDouble / (nbExpressed + nbNonExpressed))
    }

    nameToRatio.toMap
  }

  def perturbStates(
    states: Set[ConcreteBooleanState]
  ): Map[String, Set[Set[ConcreteBooleanState]]] = {
    val namesToPerturb = states.head.orderedKeys
    val nameToStates = for (name <- namesToPerturb) yield {
      val initialValuesForName = states.map(s => s.value(name))
      val complementValuesForName = complementValues(initialValuesForName)

      val perturbedStates = for (complement <- complementValuesForName) yield {
        states.map(_.replaceValue(name, complement))
      }

      name -> perturbedStates
    }
    nameToStates.toMap
  }

  def complementValues(vs: Set[Boolean]): Set[Boolean] = {
    vs.map(v => !v)
  }

}
