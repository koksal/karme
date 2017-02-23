package karme.synthesis

import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.Transitions.Transition

case class SynthesisResult(
  transitions: Set[Transition],
  functions: Set[FunExpr]
)
