package mathgraph.backend

import mathgraph.frontend.MGLTrees._
import mathgraph.util._
import BackendContext._

object Simplifier extends Pipeline[Program, Program] {

  def simplifyExpr(expr: Expr): Expr = expr match {
    case True            => Apply(trueIdentifier, Seq()).setPos(expr)
    case False           => Apply(falseIdentifier, Seq()).setPos(expr)
    case Apply(id, args) => Apply(id, args.map(simplifyExpr)).setPos(expr)
    case Not(e) =>
      simplifyExpr(e) match {
        case Not(e2) => e2.setPos(expr)
        case x =>
          Apply(impliesIdentifier, Seq(x, simplifyExpr(False))).setPos(expr)
      }
    case Implies(lhs, rhs) =>
      Apply(impliesIdentifier, Seq(simplifyExpr(lhs), simplifyExpr(rhs)))
        .setPos(expr)
    case Forall(ids1, body) =>
      simplifyExpr(body) match {
        case Forall(ids2, body2) => Forall(ids1 ++ ids2, body2).setPos(expr)
        case x                   => Forall(ids1, x).setPos(expr)
      }
  }

  // let and(A, B) = forall x. body
  // is the same as
  // let and(A, B, x) = body
  def simplifyDef(definition: Definition): Definition = definition match {
    case Let(name, vars, bodyOpt) =>
      bodyOpt match {
        case None => definition
        case Some(body) =>
          simplifyExpr(body) match {
            case Forall(seq, body2) => Let(name, vars ++ seq, Some(body2))
            case r @ _              => Let(name, vars, Some(r))
          }
      }
  }

  protected def apply(program: Program)(ctxt: Context): Program =
    program match {
      case Program(lets, axioms) =>
        Program(lets.map(simplifyDef), axioms.map(simplifyExpr))
    }
}
