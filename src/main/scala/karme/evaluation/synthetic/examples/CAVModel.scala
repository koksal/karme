package karme.evaluation.synthetic.examples

import karme.synthesis.FunctionTrees.FunAnd
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.FunctionTrees.FunNot
import karme.synthesis.FunctionTrees.FunOr
import karme.synthesis.FunctionTrees.FunVar
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.synthesis.Transitions.GenericState

object CAVModel {

  def makeSimplifiedNetwork(): Map[String, FunExpr] = {
    Map(
      "Cebpa" ->
        FunNot(
          FunOr(
            FunVar("Scl"),
            FunVar("Fog1")
          )
        ),
      "EKLF" ->
        FunAnd(
          FunVar("Gata1"),
          FunNot(
            FunVar("Fli1")
          )
        ),
      "EgrNab" ->
        FunAnd(
          FunAnd(
            FunVar("Pu_1"),
            FunVar("cJun")
          ),
          FunNot(
            FunVar("Gfi1")
          )
        ),
      "Fli1" ->
        FunAnd(
          FunVar("Gata1"),
          FunNot(
            FunVar("EKLF")
          )
        ),
      "Fog1" ->
        FunVar("Gata1"),
      "Gata1" ->
        FunNot(
          FunVar("Pu_1")
        ),
      "Gata2"	->
        FunNot(
          FunOr(
            FunVar("Pu_1"),
            FunVar ("Fog1")
          )
        ),
      "Gfi1" ->
        FunAnd(
          FunVar("Cebpa"),
          FunNot(
            FunVar("EgrNab")
          )
        ),
      "Pu_1" ->
        FunAnd(
          FunVar("Pu_1"),
          FunNot(
            FunVar("Gata2")
          )
        ),
      "Scl" ->
        FunVar("Gata1"),
      "cJun" ->
        FunAnd(
          FunVar("Pu_1"),
          FunNot(
            FunVar("Gfi1")
          )
        )
    )
  }

  def makeNetwork(): Map[String, FunExpr] = {
    Map(
      "Cebpa" ->
        FunAnd(
          FunVar("Cebpa"),
          FunNot(
            FunOr(
              FunVar("Scl"),
              FunAnd(
                FunVar("Fog1"),
                FunVar("Gata1")
              )
            )
          )
        ),
      "EKLF" ->
        FunAnd(
          FunVar("Gata1"),
          FunNot(
            FunVar("Fli1")
          )
        ),
      "EgrNab" ->
        FunAnd(
          FunAnd(
            FunVar("Pu_1"),
            FunVar("cJun")
          ),
          FunNot(
            FunVar("Gfi1")
          )
        ),
      "Fli1" ->
        FunAnd(
          FunVar("Gata1"),
          FunNot(
            FunVar("EKLF")
          )
        ),
      "Fog1" ->
        FunVar("Gata1"),
      "Gata1" ->
        FunAnd(
          FunOr(
            FunVar("Gata1"),
            FunOr(
              FunVar("Gata2"),
              FunVar("Fli1")
            )
          ),
          FunNot(
            FunVar("Pu_1")
          )
        ),
      "Gata2"	->
        FunAnd(
          FunVar("Gata2"),
          FunNot(
            FunOr(FunVar("Pu_1"),
              FunAnd(FunVar("Gata1"), FunVar ("Fog1"))
            )
          )
        ),
      "Gfi1" ->
        FunAnd(
          FunVar("Cebpa"),
          FunNot(
            FunVar("EgrNab")
          )
        ),
      "Pu_1" ->
        FunAnd(
          FunOr(
            FunVar("Cebpa"),
            FunVar("Pu_1")
          ),
          FunNot(
            FunOr(
              FunVar("Gata1"),
              FunVar("Gata2")
            )
          )
        ),
      "Scl" ->
        FunAnd(
          FunVar("Gata1"),
          FunNot(
            FunVar("Pu_1")
          )
        ),
      "cJun" ->
        FunAnd(
          FunVar("Pu_1"),
          FunNot(
            FunVar("Gfi1")
          )
        )
    )
  }

  def makeInitialState(): ConcreteBooleanState = {
    GenericState(Map(
      "Cebpa" -> true,
      "EKLF" -> false,
      "EgrNab" -> false,
      "Fli1" -> false,
      "Fog1" -> false,
      "Gata1" -> false,
      "Gata2" -> true,
      "Gfi1" -> false,
      "Pu_1" -> true,
      "Scl" -> false,
      "cJun" -> false
    ))
  }
}
