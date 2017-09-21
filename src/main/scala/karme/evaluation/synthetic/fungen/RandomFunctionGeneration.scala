package karme.evaluation.synthetic.fungen
import karme.evaluation.synthetic.topology.NetworkTopologyGraphs.NetworkTopologyGraph
import karme.synthesis.FunctionTrees
import karme.synthesis.FunctionTrees.FunAnd
import karme.synthesis.FunctionTrees.FunConst
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.FunctionTrees.FunNot
import karme.synthesis.FunctionTrees.FunOr
import karme.synthesis.FunctionTrees.FunVar

import scala.util.Random

class RandomFunctionGeneration extends FunctionGeneration {

  private val random = new Random()

  def generate(
    topology: NetworkTopologyGraph
  ): Map[String, FunctionTrees.FunExpr] = {
    val nameFunPairs = for (v <- topology.V) yield {
      val sources = topology.sources(v)
      val f = makeFunction(sources.map(_.id))
      v.id -> f
    }

    nameFunPairs.toMap
  }

  private def makeFunction(variables: Set[String]): FunExpr = {
    if (variables.isEmpty) {
      randomConstant()
    } else if (variables.size == 1) {
      randomUnary(variables.head)
    } else {
      val (leftVs, rightVs) = divideByTwo(variables)
      randomBinary(makeFunction(leftVs), makeFunction(rightVs))
    }
  }

  private def randomConstant(): FunExpr = {
    FunConst(random.nextBoolean())
  }

  private def randomUnary(v: String): FunExpr = {
    if (random.nextBoolean()) {
      FunVar(v)
    } else {
      FunNot(FunVar(v))
    }
  }

  private def randomBinary(f1: FunExpr, f2: FunExpr): FunExpr = {
    if (random.nextBoolean()) {
      FunAnd(f1, f2)
    } else {
      FunOr(f1, f2)
    }
  }

  private def divideByTwo(vs: Set[String]): (Set[String], Set[String]) = {
    require(vs.size > 1)

    val shuffled = random.shuffle(vs.toList)

    val firstHalf = shuffled.take(shuffled.size / 2)
    val secondHalf = shuffled.drop(shuffled.size / 2)

    (firstHalf.toSet, secondHalf.toSet)
  }

}
