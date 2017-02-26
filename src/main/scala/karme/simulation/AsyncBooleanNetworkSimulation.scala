package karme.simulation

import karme.synthesis.FunctionTrees
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.SynthesisResult
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.util.MathUtil

object AsyncBooleanNetworkSimulation {

  val SIMULATION_DEPTH_LIMIT = 100

  def simulateForAllCombinations(
    labelToSynthesisResults: Map[String, Set[SynthesisResult]],
    initialStates: Set[ConcreteBooleanState]
  ): Set[(Map[La])] = {

  }

  def pickFunctionsAndSimulate(
    labelToSynthesisResults: Map[String, Set[SynthesisResult]],
    initialStates: Set[ConcreteBooleanState]
  ): Set[ConcreteBooleanState] = {
    for (labelToFun <-
         enumerateSynthesisResultCombinations(labelToSynthesisResults)) {
      println("Chosen functions for simulation:")
      for ((label, fun) <- labelToFun) {
        println(s"${label}: ${fun}")
      }
      simulate(labelToFun, initialStates)
    }
  }

  /**
    * Chooses one function expression per synthesis result and returns a
    * set of all possible combinations.
    */
  def enumerateSynthesisResultCombinations(
    labelToSynthesisResults: Map[String, Set[SynthesisResult]]
  ): Set[Map[String, FunExpr]] = {
    val labels = labelToSynthesisResults.collect{
      case (label, res) if res.nonEmpty => label
    }.toList

    val orderedResultSets = labels map { l => labelToSynthesisResults(l) }
    val product = MathUtil.cartesianProduct(orderedResultSets)

    product map { synthResults =>
      // pick an arbitrary function in each synthesis result set
      val firstFunctionInEachResult = synthResults map (r => r.functions.head)
      labels.zip(firstFunctionInEachResult).toMap
    }
  }

  def simulate(
    functions: Map[String, FunExpr],
    initialStates: Set[ConcreteBooleanState]
  ): Set[ConcreteBooleanState] = {
    // for every reachable state, compute all new reachable states using any
    // applicable function.
    var reachableStates = Set.empty[ConcreteBooleanState]
    var processSet = initialStates

    var i = 0
    while (processSet.nonEmpty && i < SIMULATION_DEPTH_LIMIT) {
      i += 1
      for (stateToProcess <- processSet) {
        reachableStates += stateToProcess
        processSet -= stateToProcess

        // add any new reachable states to process set
        // apply each function and see if it generates a new state
        val statesViaFunctionApplication = functions map {
          case (label, fun) => {
            updatedState(label, fun, stateToProcess)
          }
        }

        val unseenStates = statesViaFunctionApplication.toSet --
          (reachableStates ++ processSet)

        processSet ++= unseenStates
      }
    }

    reachableStates
  }

  private def updatedState(
    label: String,
    fun: FunExpr,
    inputState: ConcreteBooleanState
  ): ConcreteBooleanState = {
    val funOutput = FunctionTrees.eval(fun, inputState)
    val newMapping = inputState.mapping.updated(label, funOutput)
    new ConcreteBooleanState(newMapping)
  }

}
