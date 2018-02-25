package karme.evaluation.synthetic.expdesign

import karme.evaluation.PerturbationAnalysis
import karme.evaluation.synthetic.examples.myeloid.MyeloidModel.KnockoutExperiment
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.Transitions.ConcreteBooleanState

trait ExperimentGuide {

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
  ): KnockoutExperiment  = {
    experiments maxBy { experiment =>
      println(s"Testing $experiment:")

      val perturbedModels = models map (m =>
        PerturbationAnalysis.knockoutVariable(m, experiment.knockoutVar))

      val perturbedInitialStates = initialStates.map(s =>
        s.replaceValue(experiment.knockoutVar, false))

      val evaluationValues = perturbedModels map (m =>
        evaluateModel(m, perturbedInitialStates))

      val maxPairwiseDist = maxPairwiseDistance(evaluationValues)
      println(s"Maximum pairwise distance: $maxPairwiseDist")
      maxPairwiseDist
    }
  }

}

