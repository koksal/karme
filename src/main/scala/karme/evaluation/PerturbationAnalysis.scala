package karme.evaluation

import karme.Clustering
import karme.Reporter
import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.simulation.AsyncBooleanNetworkSimulation
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.FunctionTrees.FunVar
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.visualization.StateGraphPlotter

/**
  * Perturbs initial states and functions, and assesses reachability of genes
  * of interest.
  */
class PerturbationAnalysis(
  labelToOriginalFunction: Map[String, FunExpr],
  graph: DirectedBooleanStateGraph,
  initialStates: Set[ConcreteBooleanState],
  clustering: Clustering,
  namesToObserve: Set[String]
)(implicit reporter: Reporter) {

  private val stateGraphPlotter = new StateGraphPlotter(reporter)

  def findGeneDrivers() = {
    val originalRatios = computeExpressedStateRatios(labelToOriginalFunction,
      initialStates, "simulation-original")

    for {
      nameToPerturb <- initialStates.head.orderedKeys
      value <- List(true, false)
    } {
      println(s"Fixing $nameToPerturb to $value")
      val perturbedRatios = getRatiosForPerturbedName(nameToPerturb, value)

      val ratioDifferences = computeRatioDifferences(originalRatios,
        perturbedRatios)

      for ((observedName, diff) <- ratioDifferences.toList.sortBy(- _._2)) {
        println(s"Expression ratio difference for $observedName: $diff")
      }
    }
  }

  def getRatiosForPerturbedName(
    name: String,
    value: Boolean
  ) = {
    val overriddenStates = overrideStatesWithInitialValue(initialStates,
      name, value)

    val overriddenFunctions = overrideWithIdentityFunction(
      labelToOriginalFunction, name)

    computeExpressedStateRatios(overriddenFunctions, overriddenStates,
      s"simulation-$name-$value")
  }

  def overrideWithIdentityFunction(
    labelToFunction: Map[String, FunExpr],
    nameToOverride: String
  ): Map[String, FunExpr] = {
    val identityFunction = FunVar(nameToOverride)
    labelToFunction.updated(nameToOverride, identityFunction)
  }

  def overrideStatesWithInitialValue(
    states: Set[ConcreteBooleanState],
    name: String,
    value: Boolean
  ): Set[ConcreteBooleanState] = {
    states map (s => s.replaceValue(name, value))
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
    labelToFunction: Map[String, FunExpr],
    initialStates: Set[ConcreteBooleanState],
    plotLabel: String
  ): Map[String, Double] = {
    val reachedStates = AsyncBooleanNetworkSimulation.simulate(labelToFunction,
      initialStates)

    stateGraphPlotter.plotDirectedGraph(graph, plotLabel,
      nodeHighlightGroups = List(initialStates, reachedStates))

    val nameToRatio = for (name <- initialStates.head.orderedKeys) yield {
      val nbExpressed = reachedStates.count(s => s.value(name))
      val nbNonExpressed = reachedStates.count(s => !s.value(name))
      assert(nbExpressed + nbNonExpressed == reachedStates.size)

      name -> (nbExpressed.toDouble / (nbExpressed + nbNonExpressed))
    }

    nameToRatio.toMap
  }

}
