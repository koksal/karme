package karme.evaluation.synthetic.examples.myeloid

import karme.evaluation.synthetic.stategen.ExhaustiveStateEnumeration
import karme.synthesis.FunctionTrees.FunAnd
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.FunctionTrees.FunNot
import karme.synthesis.FunctionTrees.FunOr
import karme.synthesis.FunctionTrees.FunVar
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.synthesis.Transitions.GenericState

object MyeloidModel {

  val Cebpa = "Cebpa"
  val EKLF = "EKLF"
  val EgrNab = "EgrNab"
  val Fli1 = "Fli1"
  val Fog1 = "Fog1"
  val Gata1 = "Gata1"
  val Gata2 = "Gata2"
  val Gfi1 = "Gfi1"
  val Pu_1 = "Pu_1"
  val Scl = "Scl"
  val cJun = "cJun"

  val s1 = "s1"
  val s2 = "s2"
  val s3 = "s3"
  val s4 = "s4"

  val allCellTypeIDs = Set(s1, s2, s3, s4)

  val stableStates: Set[ConcreteBooleanState] = {
    namedStableStates().values.toSet
  }

  val allStates: Set[ConcreteBooleanState] = {
    new ExhaustiveStateEnumeration(makeInitialState().orderedKeys)
      .enumerateAllStates()
  }

  def namedStableStates(): Map[String, ConcreteBooleanState] = {
    Map(
      "s1" ->
        GenericState(
          Map(
            Gata2 -> false,
            Gata1 -> true,
            Fog1 -> true,
            EKLF -> true,
            Fli1 -> false,
            Scl -> true,
            Cebpa -> false,
            Pu_1 -> false,
            cJun -> false,
            EgrNab -> false,
            Gfi1 -> false
          )
        ),
      "s2" ->
        GenericState(
          Map(
            Gata2 -> false,
            Gata1 -> true,
            Fog1 -> true,
            EKLF -> false,
            Fli1 -> true,
            Scl -> true,
            Cebpa -> false,
            Pu_1 -> false,
            cJun -> false,
            EgrNab -> false,
            Gfi1 -> false
          )
        ),
      "s3" ->
        GenericState(
          Map(
            Gata2 -> false,
            Gata1 -> false,
            Fog1 -> false,
            EKLF -> false,
            Fli1 -> false,
            Scl -> false,
            Cebpa -> true,
            Pu_1 -> true,
            cJun -> true,
            EgrNab -> true,
            Gfi1 -> false
          )
        ),
      "s4" ->
        GenericState(
          Map(
            Gata2 -> false,
            Gata1 -> false,
            Fog1 -> false,
            EKLF -> false,
            Fli1 -> false,
            Scl -> false,
            Cebpa -> true,
            Pu_1 -> true,
            cJun -> false,
            EgrNab -> false,
            Gfi1 -> true
          )
        )
    )
  }

  def makePLOSNetwork(): Map[String, FunExpr] = {
    makeCAVNetwork().updated(Cebpa,
      FunAnd(
        FunVar(Cebpa),
        FunNot(
          FunAnd(
            FunVar(Scl),
            FunAnd(
              FunVar(Fog1),
              FunVar(Gata1)
            )
          )
        )
      )
    )
  }

  def makeCAVNetwork(): Map[String, FunExpr] = {
    Map(
      Cebpa ->
        FunAnd(
          FunVar(Cebpa),
          FunNot(
            FunOr(
              FunVar(Scl),
              FunAnd(
                FunVar(Fog1),
                FunVar(Gata1)
              )
            )
          )
        ),
      EKLF ->
        FunAnd(
          FunVar(Gata1),
          FunNot(
            FunVar(Fli1)
          )
        ),
      EgrNab ->
        FunAnd(
          FunAnd(
            FunVar(Pu_1),
            FunVar(cJun)
          ),
          FunNot(
            FunVar(Gfi1)
          )
        ),
      Fli1 ->
        FunAnd(
          FunVar(Gata1),
          FunNot(
            FunVar(EKLF)
          )
        ),
      Fog1 ->
        FunVar(Gata1),
      Gata1 ->
        FunAnd(
          FunOr(
            FunVar(Gata1),
            FunOr(
              FunVar(Gata2),
              FunVar(Fli1)
            )
          ),
          FunNot(
            FunVar(Pu_1)
          )
        ),
      Gata2	->
        FunAnd(
          FunVar(Gata2),
          FunNot(
            FunOr(FunVar(Pu_1),
              FunAnd(FunVar(Gata1), FunVar (Fog1))
            )
          )
        ),
      Gfi1 ->
        FunAnd(
          FunVar(Cebpa),
          FunNot(
            FunVar(EgrNab)
          )
        ),
      Pu_1 ->
        FunAnd(
          FunOr(
            FunVar(Cebpa),
            FunVar(Pu_1)
          ),
          FunNot(
            FunOr(
              FunVar(Gata1),
              FunVar(Gata2)
            )
          )
        ),
      Scl ->
        FunAnd(
          FunVar(Gata1),
          FunNot(
            FunVar(Pu_1)
          )
        ),
      cJun ->
        FunAnd(
          FunVar(Pu_1),
          FunNot(
            FunVar(Gfi1)
          )
        )
    )
  }

  def makeTrimmedStateSpaceNetwork(): Map[String, FunExpr] = {
    Map(
      Cebpa ->
        FunOr(
          FunNot(
            FunVar(Fog1)
          ),
          FunNot(
            FunVar(Scl)
          )
        ),
      EKLF ->
        FunAnd(
          FunNot(
            FunVar(Fli1)
          ),
          FunVar(Gata1)
        ),
      EgrNab ->
        FunVar(cJun),
      Fli1 ->
        FunAnd(
          FunNot(
            FunVar(EKLF)
          ),
          FunVar(Gata1)
        ),
      Fog1 ->
        FunVar(Gata1),
      Gata1 ->
        FunNot(
          FunVar(Pu_1)
        ),
      Gata2 ->
        FunAnd(
          FunNot(
            FunVar(Fog1)
          ),
          FunNot(
            FunVar(Pu_1)
          )
        ),
      Gfi1 ->
        FunAnd(
          FunNot(
            FunVar(cJun)
          ),
          FunVar(Pu_1)
        ),
      Pu_1 ->
        FunOr(
          FunAnd(
            FunNot(
              FunVar(Fog1)
            ),
            FunNot(
              FunVar(Gata2)
            )
          ),
          FunOr(
            FunVar(Gfi1),
            FunVar(cJun)
          )
        ),
      Scl ->
        FunVar(Gata1),
      cJun ->
        FunAnd(
          FunNot(
            FunVar(Gfi1)
          ),
          FunVar(Pu_1)
        )
    )
  }

  def makeInitialState(): ConcreteBooleanState = {
    GenericState(Map(
      Cebpa -> true,
      EKLF -> false,
      EgrNab -> false,
      Fli1 -> false,
      Fog1 -> false,
      Gata1 -> false,
      Gata2 -> true,
      Gfi1 -> false,
      Pu_1 -> true,
      Scl -> false,
      cJun -> false
    ))
  }

  case class KnockoutExperiment(
    knockoutVar: String,
    observedOriginalAttractors: Set[String],
    nbNewAttractors: Int
  )

  def knockoutExperiments(): Seq[KnockoutExperiment] = {
    val experimentsWithEvidence = Seq(
      KnockoutExperiment(Gata1, Set(s3, s4), 1),
      KnockoutExperiment(Fog1, Set(s3, s4), 4),
      KnockoutExperiment(EKLF, Set(s2, s3, s4), 0),
      KnockoutExperiment(Fli1, Set(s1, s3, s4), 0),
      KnockoutExperiment(Cebpa, Set(s1, s2), 0),
      KnockoutExperiment(Pu_1, Set(s1, s2), 0),
      KnockoutExperiment(EgrNab, Set(s1, s2, s4), 1),
      KnockoutExperiment(Gfi1, Set(s1, s2, s3), 0)
    )

    val experimentsWithoutEvidence = Seq(
      KnockoutExperiment(Gata2, Set(s3, s4), 0),
      KnockoutExperiment(Scl, Set(s3, s4), 2),
      KnockoutExperiment(cJun, Set(s1, s2, s4), 0)
    )

    experimentsWithEvidence ++ experimentsWithoutEvidence
  }
}
