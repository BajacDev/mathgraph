package mathgraph.backend

import mathgraph.frontend.MGLTrees._
import mathgraph.util._

// every forall will be transformed into a Simplelet
// eg not(forall x. not(forall y. f(x, y)))
// become
// let __859374__(x, y) = f(x, y)
// let __203923__(x) = not(__859374__(x))
// not(__203923__)
// todo: must use special Identifier instead of hashes
object ForallToLets extends Pipeline[Program, Program] {

  def createLetName(expr: Expr): String = s"__${expr.hashCode}__"

  def extractDefFromExpr(
      expr: Expr,
      freeVars: Seq[Identifier]
  ): (Expr, Seq[Let]) = expr match {
    case Apply(id, args) => {
      val result = args.map(extractDefFromExpr(_, freeVars))
      val newApply = Apply(id, result.map(_._1))
      val newLets = result.flatMap(_._2)
      (newApply, newLets)
    }
    case Forall(seq, body) => {
      val newFreeVars = freeVars ++ seq
      val (letBody, lets) = extractDefFromExpr(body, newFreeVars)
      val letName = createLetName(expr)
      val newLet = Let(letName, newFreeVars, Some(letBody))
      val newExpr = Apply(letName, freeVars.map(Apply(_, Seq())))
      (newExpr, lets :+ newLet)
    }
  }

  def extractDefFromAxiom(expr: Expr): (Expr, Seq[Let]) =
    extractDefFromExpr(expr, Seq())

  def extractDefFromDef(definition: Definition): Seq[Definition] =
    definition match {
      case r @ Let(name, vars, bodyOpt) =>
        bodyOpt match {
          case None => Seq(r)
          case Some(body) => {
            val (expr, lets) = extractDefFromExpr(body, vars)
            lets :+ Let(name, vars, Some(expr))
          }
        }
    }

  protected def apply(program: Program)(ctxt: Context): Program =
    program match {
      case Program(lets, axioms) => {
        val result = axioms.map(extractDefFromAxiom)
        val newAxioms = result.map(_._1)
        val newLets = lets.flatMap(extractDefFromDef) ++ result.flatMap(_._2)
        Program(newLets, newAxioms)
      }
    }
}
