package karme.synthesis

import karme.synthesis.Trees._

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
            case _ => {
              sys.error("Unexpected model value.")
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
