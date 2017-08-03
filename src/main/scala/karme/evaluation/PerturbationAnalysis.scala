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
  targetsOfInterest: Set[String],
  expectedDrivers: Set[String]
)(implicit reporter: Reporter) {

  case class Perturbation(name: String, value: Boolean)
  case class PerturbationEffect(
    perturbation: Perturbation,
    target: String,
    expressedStateRatioDiff: Double
  )

  private val stateGraphPlotter = new StateGraphPlotter(reporter)

  def findGeneDrivers() = {
    val originalRatios = computeExpressedStateRatios(labelToOriginalFunction,
      initialStates, "simulation-original")

    val effects = getPerturbationEffects(originalRatios)

    analyzeEffects(effects)
  }

  def analyzeEffects(effects: Seq[PerturbationEffect]) = {
    val targetClusterRatios = geneRatiosInClusters(targetsOfInterest)
    val expectedDriverClusterRatios = geneRatiosInClusters(expectedDrivers)

    println(s"Expected driver cluster ratios: $expectedDriverClusterRatios")

    val reverseOrderedTargetClusterRatios = targetClusterRatios.toList.sortBy{
      case (c, r) => - r
    }

    for ((cluster, targetRatio) <- reverseOrderedTargetClusterRatios) {
      println(s"Ratio of targets in $cluster: $targetRatio")

      val targetEffects = effects.filter(e => e.target == cluster)
      val targetEffectsByDescRatio = targetEffects.sortBy(
        e => - e .expressedStateRatioDiff)

      for (effect <- targetEffectsByDescRatio
           if effect.perturbation.name != cluster) {
        println(s"Effect of ${effect.perturbation}: " +
          s"${effect.expressedStateRatioDiff}")

        println(s"Expected driver gene ratio in ${effect.perturbation.name}: " +
          s"${expectedDriverClusterRatios.get(effect.perturbation.name)}")
      }
    }
  }

  def geneRatiosInClusters(genes: Set[String]): Map[String, Double] = {
    val clustOptToGenes = genes.groupBy(g => clustering.memberToCluster.get(g))
    clustOptToGenes.collect{
      case (Some(clusterName), genesInCluster) => {
        val geneRatioInCluster = genesInCluster.size.toDouble / genes.size
        (clusterName, geneRatioInCluster)
      }
    }
  }

  def getPerturbationEffects(
    originalRatios: Map[String, Double]
  ): Seq[PerturbationEffect] = {
    val effectSets = for {
      perturbation <- allPerturbations
    } yield {
      println(s"Fixing ${perturbation.name} to ${perturbation.value}")
      val perturbedRatios = getRatiosForPerturbation(perturbation)

      val ratioDifferences = computeRatioDifferences(originalRatios,
        perturbedRatios)

      for ((observedName, diff) <- ratioDifferences) yield {
        PerturbationEffect(perturbation, observedName, diff)
      }
    }
    effectSets.flatten
  }

  def allPerturbations: Seq[Perturbation] = {
    val allNames = initialStates.head.orderedKeys

    for {
      name <- allNames
      truthValue <- List(true, false)
    } yield {
      Perturbation(name, truthValue)
    }
  }

  def getRatiosForPerturbation(
    perturbation: Perturbation
  ): Map[String, Double] = {
    val overriddenStates = overrideStatesWithInitialValue(initialStates,
      perturbation.name, perturbation.value)

    val overriddenFunctions = overrideWithIdentityFunction(
      labelToOriginalFunction, perturbation.name)

    computeExpressedStateRatios(overriddenFunctions, overriddenStates,
      s"simulation-${perturbation.name}-${perturbation.value}")
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
