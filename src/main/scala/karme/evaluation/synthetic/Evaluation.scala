package karme.evaluation.synthetic

import karme.evaluation.synthetic.fungen.RandomFunctionGeneration
import karme.evaluation.synthetic.stategen.RandomStateGeneration
import karme.evaluation.synthetic.topology.LinearNetworkGeneration

object Evaluation {

  def main(args: Array[String]): Unit = {

  }

  def runSyntheticWorkflow(): Unit = {
    // 1. Create network topology
    val topology = new LinearNetworkGeneration(10).generate()

    // 2. Create functions for each node
    val labelToFun = new RandomFunctionGeneration().generate(topology)

    // 3. Pick an initial state
    val initialState = new RandomStateGeneration(labelToFun.keySet).generate()

    // 4. Simulate network
    // TODO collect time stamps in simulation

    // 5. Optionally alter simulated data (sample, flip bits)
    // 6. Run inference
    // 7. Compare synthesized functions against original functions (visualize inferred GRN)
  }

}
