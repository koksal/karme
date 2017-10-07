package karme.evaluation

import karme.{Clustering, Reporter}
import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.simulation.AsyncBooleanNetworkSimulation
import karme.synthesis.FunctionTrees
import karme.synthesis.FunctionTrees.FunConst
import karme.synthesis.FunctionTrees.{FunExpr, FunVar}
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.util.MathUtil
import karme.visualization.graph.StateGraphPlotter

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

  case class Perturbation(name: String, value: Boolean) {
    override def toString: String = s"Perturbation: $name = $value"
  }
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

    val reverseOrderedTargetClusterRatios = targetClusterRatios.toList.sortBy{
      case (c, r) => - r
    }

    for ((cluster, targetRatio) <- reverseOrderedTargetClusterRatios) {
      println(
        s"${ratioToPercentage(targetRatio)} of target genes are in $cluster: ")
      print(
        genesInCluster(targetsOfInterest, cluster).toList.sorted.mkString(", "))
      println()

      val targetEffects = effects.filter(e => e.target == cluster)
      val targetEffectsByDescRatio = targetEffects.sortBy(
        e => - e .expressedStateRatioDiff)

      for (effect <- targetEffectsByDescRatio
           if effect.perturbation.name != cluster) {
        println(
            s"${ratioToPercentage(effect.expressedStateRatioDiff)} change in " +
            s"ratio of $cluster-expressed reachable cells for " +
              s"${effect.perturbation}")

        val expectedGeneRatioInEffectSourceCluster =
          expectedDriverClusterRatios.getOrElse(effect.perturbation.name, 0.0)
        println(
          s"${ratioToPercentage(expectedGeneRatioInEffectSourceCluster)} " +
          s"of expected driver genes are in ${effect.perturbation.name}: ")
        println(
          genesInCluster(expectedDrivers, effect.perturbation.name).toList
            .sorted.mkString(", "))
        println()
      }
      println()
    }
  }

  def geneRatiosInClusters(
    genes: Set[String]
  ): Map[String, Double] = {
    val clustOptToGenes = genes.groupBy(g => clustering.memberToCluster.get(g))
    clustOptToGenes.collect{
      case (Some(clusterName), genesInCluster) => {
        val geneRatioInCluster =
          genesInCluster.size.toDouble / genes.size
        (clusterName, geneRatioInCluster)
      }
    }
  }

  def genesInCluster(genes: Set[String], cluster: String): Set[String] = {
    val clustMembers = clustering.clusterToMembers(cluster)
    clustMembers intersect genes
  }

  def getPerturbationEffects(
    originalRatios: Map[String, Double]
  ): Seq[PerturbationEffect] = {
    val effectSets = for {
      perturbation <- allPerturbations
    } yield {
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

    val overriddenFunctions = PerturbationAnalysis
      .overrideWithIdentityFunction(labelToOriginalFunction, perturbation.name)

    computeExpressedStateRatios(overriddenFunctions, overriddenStates,
      s"simulation-${perturbation.name}-${perturbation.value}")
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

  def ratioToPercentage(r: Double): String = {
    s"${MathUtil.roundTo(2)(r * 100)}%"
  }
}

object PerturbationAnalysis {

  def overrideWithIdentityFunction(
    labelToFunction: Map[String, FunExpr],
    nameToOverride: String
  ): Map[String, FunExpr] = {
    val identityFunction = FunVar(nameToOverride)
    labelToFunction.updated(nameToOverride, identityFunction)
  }

  def overrideWithConstantFunction(
    labelToFunction: Map[String, FunExpr],
    nameToOverride: String,
    constantValue: Boolean
  ): Map[String, FunExpr] = {
    val constFunction = FunConst(constantValue)
    labelToFunction.updated(nameToOverride, constFunction)
  }

  def knockoutVariable(
    labelToFunction: Map[String, FunExpr],
    label: String
  ): Map[String, FunExpr] = {
    overrideWithConstantFunction(labelToFunction, label, false)
  }

  def overExpressVariable(
    labelToFunction: Map[String, FunExpr],
    label: String
  ): Map[String, FunExpr] = {
    overrideWithConstantFunction(labelToFunction, label, true)
  }

  def knockoutInteraction(fe: FunExpr, v: String): FunExpr = {
    FunctionTrees.replaceVar(fe, v, FunConst(false))
  }

}
