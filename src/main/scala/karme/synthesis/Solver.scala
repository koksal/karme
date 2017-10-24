package karme.synthesis

import karme.synthesis.Trees._
import karme.synthesis.TypeTrees._
import karme.util.TimingUtil
import z3.scala.{Z3AST, Z3Context, Z3Model, Z3Solver, Z3Sort}

class Solver {
  private var neverInitialized = true

  private var ctx: Z3Context = null
  private var z3Solver: Z3Solver = null
  private var z3Vars: Map[Identifier, Z3AST] = null

  private val USEBV   = false
  private val BVWIDTH = -1

  /** Restarts by instantiating a new context. */
  private def restart(): Unit = {
    if (neverInitialized) {
      neverInitialized = false
    } else {
      ctx.delete
    }

    ctx = new Z3Context("MODEL" -> true)
    z3Solver = ctx.mkSolver()
    z3Vars = Map.empty
  }

  def check(expr: Expr): Option[Map[Identifier, Expr]] = {
    restart()
    z3Solver.assertCnstr(toZ3Formula(expr))
    TimingUtil.time("Z3 dispatch") {
      z3Solver.check() match {
        case Some(true) => Some(modelToMap(z3Solver.getModel()))
        case _ => None
      }
    }
  }

  private def modelToMap(m: Z3Model): Map[Identifier, Expr] = {
    var model: Map[Identifier, Expr] = Map.empty
    for ((id, ast) <- z3Vars) {
      id.getType match {
        case Untyped => sys.error("Untyped expr: " + id)
        case IntType => m.evalAs[Int](ast) match {
          case Some(v) => 
            model += ((id, IntLiteral(v)))
          case None =>
        }
        case BooleanType => m.evalAs[Boolean](ast) match {
          case Some(v) =>
            model += ((id, BooleanLiteral(v)))
          case None =>
        }
      }
    }
    model
  }

  private def intSort: Z3Sort = {
    if (USEBV)
      ctx.mkBVSort(BVWIDTH)
    else
      ctx.mkIntSort
  }

  private def booleanSort: Z3Sort = ctx.mkBoolSort

  private def typeToSort(tt: TypeTree): Z3Sort = tt match {
    case Untyped => sys.error("Translating untyped expression")
    case IntType => intSort
    case BooleanType => booleanSort
  }

  private def toZ3Formula(expr: Expr): Z3AST = expr match {
    case And(exprs) => ctx.mkAnd(exprs.map(toZ3Formula(_)): _*)
    case Or(exprs) => ctx.mkOr(exprs.map(toZ3Formula(_)): _*)
    case Iff(l, r) => ctx.mkIff(toZ3Formula(l), toZ3Formula(r))
    case Implies(l, r) => ctx.mkImplies(toZ3Formula(l), toZ3Formula(r))
    case Not(e) => ctx.mkNot(toZ3Formula(e))
    case Equals(l, r) => ctx.mkEq(toZ3Formula(l), toZ3Formula(r))
    case Plus(l, r) => 
      val nl = toZ3Formula(l)
      val nr = toZ3Formula(r)
      if (USEBV) ctx.mkBVAdd(nl, nr) else ctx.mkAdd(nl, nr)
    case LessThan(l, r) => 
      val nl = toZ3Formula(l)
      val nr = toZ3Formula(r)
      if (USEBV) ctx.mkBVSlt(nl, nr) else ctx.mkLT(nl, nr)
    case GreaterThan(l, r) =>
      val nl = toZ3Formula(l)
      val nr = toZ3Formula(r)
      if (USEBV) ctx.mkBVSgt(nl, nr) else ctx.mkGT(nl, nr)
    case LessEquals(l, r) =>
      val nl = toZ3Formula(l)
      val nr = toZ3Formula(r)
      if (USEBV) ctx.mkBVSle(nl, nr) else ctx.mkLE(nl, nr)
    case GreaterEquals(l, r) =>
      val nl = toZ3Formula(l)
      val nr = toZ3Formula(r)
      if (USEBV) ctx.mkBVSge(nl, nr) else ctx.mkGE(nl, nr)
    case ITE(cond, thn, els) =>
      ctx.mkITE(toZ3Formula(cond), toZ3Formula(thn), toZ3Formula(els))
    case v @ Variable(id) => z3Vars.get(id) match {
      case Some(ast) => ast
      case None =>
        val newAST = ctx.mkFreshConst(id.uniqueName, typeToSort(v.getType))
        z3Vars = z3Vars + (id -> newAST)
        newAST
    }
    case IntLiteral(v) => ctx.mkInt(v, intSort)
    case BooleanLiteral(v) => if (v) ctx.mkTrue else ctx.mkFalse
  }

}
