package karme.evaluation.synthetic.expdesign

import karme.Reporter
import karme.evaluation.PerturbationAnalysis
import karme.evaluation.synthetic.examples.myeloid.MyeloidModel.KnockoutExperiment
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.util.TSVUtil

trait ExperimentGuide {

  val reporter: Reporter
  val id: String

  type EvalDomain

  def evaluateModel(
    m: Map[String, FunExpr],
    initialStates: Set[ConcreteBooleanState]
  ): EvalDomain

  def distance(t1: EvalDomain, t2: EvalDomain): Double

  def maxPairwiseDistance(ts: Seq[EvalDomain]): Double = {
    val (t1, t2) = ts.zip(ts) maxBy {
      case (t1, t2) => distance(t1, t2)
    }

    distance(t1, t2)
  }

  def mostDistinguishingExperiment(
    experiments: Seq[KnockoutExperiment],
    models: Seq[Map[String, FunExpr]],
    initialStates: Set[ConcreteBooleanState]
  ): (KnockoutExperiment, Double)  = {
    val expToMaxPairwiseDist = experiments map { experiment =>
      println(s"Testing experiment ${experiment.knockoutVar}")

      val perturbedModels = models map (m =>
        PerturbationAnalysis.knockoutVariable(m, experiment.knockoutVar))

      val perturbedInitialStates = initialStates.map(s =>
        s.replaceValue(experiment.knockoutVar, false))

      val evaluationValues = perturbedModels map (m =>
        evaluateModel(m, perturbedInitialStates))

      val maxPairwiseDist = maxPairwiseDistance(evaluationValues)
      experiment -> maxPairwiseDist
    }

    logExperimentDistances(expToMaxPairwiseDist.toMap)

    expToMaxPairwiseDist.maxBy(_._2)
  }

  private def logExperimentDistances(
    expToDist: Map[KnockoutExperiment, Double]
  ) : Unit = {
    val varHeader = "Variable"
    val maxDistHeader = "Maximum pairwise distance"

    val tuples = expToDist.toList map {
      case (exp, dist) => {
        Map(varHeader -> exp.knockoutVar, maxDistHeader -> dist)
      }
    }

    TSVUtil.saveTupleMapsWithOrderedHeaders(
      List(varHeader, maxDistHeader),
      tuples,
      reporter.file(s"max-pairwise-model-distance-$id.tsv")
    )
  }

}

