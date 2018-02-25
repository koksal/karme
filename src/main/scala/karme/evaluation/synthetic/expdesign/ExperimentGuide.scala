package karme.evaluation.synthetic.expdesign

import karme.evaluation.PerturbationAnalysis
import karme.evaluation.synthetic.examples.myeloid.MyeloidModel.KnockoutExperiment
import karme.synthesis.FunctionTrees.FunExpr

trait ExperimentGuide[T] {

  def evaluateModel(m: Map[String, FunExpr]): T

  def distance(t1: T, t2: T): Double

  def maxPairwiseDistance(ts: Seq[T]): Double = {
    val (t1, t2) = ts.zip(ts) maxBy {
      case (t1, t2) => distance(t1, t2)
    }

    distance(t1, t2)
  }

  def mostDistinguishingExperiment(
    experiments: Seq[KnockoutExperiment],
    models: Seq[Map[String, FunExpr]]
  ): KnockoutExperiment  = {
    experiments maxBy { experiment =>
      val perturbedModels = models map (m =>
        PerturbationAnalysis.knockoutVariable(m, experiment.knockoutVar))

      val evaluationValues = perturbedModels map evaluateModel

      maxPairwiseDistance(evaluationValues)
    }
  }

}

