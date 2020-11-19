package mathgraph.backend

import mathgraph.frontend.Trees._
import mathgraph.util._
import BackendContext._

object Simplifier extends Pipeline[Program, Program] {

  def simplifyExpr(expr: Expr): Expr = expr match {
    case True            => Apply(trueIdentifier, Seq())
    case False           => Apply(falseIdentifier, Seq())
    case Apply(id, args) => Apply(id, args.map(simplifyExpr))
    case Implies(lhs, False) =>
      simplifyExpr(lhs) match {
        case Implies(lhs2, False) => lhs2
        case r @ _                => Apply(impliesIdentifier, Seq(r, simplifyExpr(False)))
      }
    case Implies(lhs, rhs) =>
      Apply(impliesIdentifier, Seq(simplifyExpr(lhs), simplifyExpr(rhs)))
    case Forall(id, body) =>
      simplifyExpr(body) match {
        case MultiForall(seq, body2) => MultiForall(id +: seq, body2)
        case r @ _                   => MultiForall(Seq(id), r)
      }
  }

  // let and(A, B) = forall x. body
  // is the same as
  // let and(A, B, x) = body
  def simplifyDef(definition: Def): Def = definition match {
    case Let(name, vars, bodyOpt) =>
      bodyOpt match {
        case None => definition
        case Some(body) =>
          simplifyExpr(body) match {
            case MultiForall(seq, body2) => Let(name, vars ++ seq, Some(body2))
            case r @ _                   => Let(name, vars, Some(r))
          }
      }
  }

  protected def apply(program: Program)(ctxt: Context): Program =
    program match {
      case Program(lets, axioms) =>
        Program(lets.map(simplifyDef), axioms.map(simplifyExpr))
    }
}
