package karme.synthesis

import karme.synthesis.Trees.And
import karme.synthesis.Trees.BooleanLiteral
import karme.synthesis.Trees.Expr
import karme.synthesis.Trees.Identifier
import karme.synthesis.Trees.IntLiteral
import karme.synthesis.Trees.LessThan
import karme.synthesis.Trees.Variable

object Optimization {

  def minimize[T](
    objective: Identifier,
    constraints: Expr
  ): Option[Int] = {
    // TODO exponentially move the search

    val solver = new Solver()
    var currentObjective: Option[Int] = None
    var reachedUNSAT = false

    def updateObjective(newValue: Int): Unit = currentObjective match {
      case None => {
        currentObjective = Some(newValue)
      }
      case Some(oldValue) => {
        assert(newValue < oldValue)
        currentObjective = Some(newValue)
      }
    }

    def objectiveImprovementConstraint(): Expr = currentObjective match {
      case None => BooleanLiteral(true)
      case Some(currentValue) =>
        LessThan(Variable(objective), IntLiteral(currentValue))
    }

    while (!reachedUNSAT) {
      val toCheck = And(constraints, objectiveImprovementConstraint())
      solver.check(toCheck) match {
        case Some(model) => {
          model(objective) match {
            case IntLiteral(objectiveValue) => {
              updateObjective(objectiveValue)
            }
          }
        }
        case None => {
          reachedUNSAT = true
        }
      }
    }

    currentObjective
  }

}
