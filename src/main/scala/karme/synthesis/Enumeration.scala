package karme.synthesis

import karme.synthesis.Trees._

object Enumeration {

  def enumerate[T](
    extractValue: (Map[Identifier, Expr] => T), 
    symbolicEquals: (T => Expr),
    constraints: Expr,
    limit: Option[Int]
  ): List[T] = {
    val solver = new Solver()
    var reachedUNSAT = false
    var sols = List[T]()

    def reachedLimit() = limit match {
      case Some(l) => sols.size >= l
      case None => false
    }

    while (!reachedUNSAT && !reachedLimit()) {
      val diffFromPrev = And(sols.map(sol => Not(symbolicEquals(sol))): _*)
      val toCheck = And(constraints, diffFromPrev)
      solver.check(toCheck) match {
        case Some(model)  => 
          sols = sols :+ extractValue(model)
        case None => 
          reachedUNSAT = true
      }
    }
    sols
  }

}
