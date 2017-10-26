package karme.synthesis

import karme.synthesis.Trees._

object Enumeration {

  def enumerate[T](
    extractValue: (Map[Identifier, Expr] => T), 
    symbolicEquals: (T => Expr),
    constraints: Expr,
    limit: Option[Int]
  ): Iterator[T] = {
    new Iterator[T] {
      private val solver = new Solver()
      private var lastSol: Option[Option[T]] = None
      private var sols = List[T]()

      private def reachedLimit() = limit match {
        case Some(l) => sols.size >= l
        case None => false
      }

      def hasNext: Boolean = lastSol match {
        case None => {
          !reachedLimit() && {
            val diffFromPrev = And(
              sols.map(sol => Not(symbolicEquals(sol))): _*)
            val toCheck = And(constraints, diffFromPrev)

            solver.check(toCheck) match {
              case Some(model)  =>
                val sol = extractValue(model)
                lastSol = Some(Some(sol))
                sols = sols :+ sol
                true
              case None =>
                lastSol = Some(None)
                false
            }
          }
        }
        case Some(None) => false
        case Some(Some(_)) => true
      }

      def next(): T = lastSol match {
        case None => {
          throw new Exception(
            "Called next on an iterator without calling hasNext before it.")
        }
        case Some(None) => {
          throw new Exception("Called next while there is no next element.")
        }
        case Some(Some(v)) => {
          lastSol = None
          v
        }
      }
    }
  }

}
