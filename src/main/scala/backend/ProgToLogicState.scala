package mathgraph.backend

import mathgraph.frontend._
import mathgraph.repl._
import mathgraph.corelogic.LogicGraph
import mathgraph.frontend.BackendTrees._
import mathgraph.util._
import mathgraph.util.{Context => UtilContext}
import mathgraph.backend.{BackendContext => Context}
import mathgraph.printer._
import mathgraph.solver._

object ProgToLogicState extends Pipeline[Program, LogicState] {

  /* return global identifiers in expr
   * exemple true -> false has Set(true, false, ->)
   * as global identifiers
   */
  def globalIdentifiers(
      expr: Expr,
      allGlobalIds: Set[Name]
  ): Set[Name] = expr match {
    case Apply(id, args) => {
      val set: Set[Name] =
        if (allGlobalIds.contains(id)) Set(id)
        else Set()
      args.foldLeft(set) { case (s, arg) =>
        s ++ globalIdentifiers(arg, allGlobalIds)
      }
    }
  }

  def exprToLogicGraph(
      expr: Expr,
      stringToExpr: Map[Name, Int],
      lg: LogicGraph
  ): Int = expr match {
    case Apply(id, args) => {
      var pos = stringToExpr(id)
      for (arg <- args) {
        val argPos = exprToLogicGraph(arg, stringToExpr, lg)
        pos = lg.fix(pos, argPos)
      }
      pos
    }
  }

  def interpretExpr(
      expr: Expr,
      context: Context,
      vars: Seq[Name]
  ): Int = {
    val Context(lg, globalstringToExpr) = context
    if (vars.length == 0) {
      exprToLogicGraph(expr, globalstringToExpr, lg)
    } else {
      val globals = globalIdentifiers(expr, globalstringToExpr.keySet).toSeq
      val freeVar = globals ++ vars
      val localStringToExpr = freeVar.zipWithIndex.map { case (id, symbolId) =>
        (id, lg.idToSymbol(symbolId))
      }.toMap
      val pos = exprToLogicGraph(expr, localStringToExpr, lg)

      globals.foldLeft(lg.forall(pos)) { case (pos2, arg) =>
        lg.fix(pos2, globalstringToExpr(arg))
      }
    }
  }

  def interpretAxioms(expr: Expr, context: Context): Unit = {
    val pos = interpretExpr(expr, context, Seq())
    context.logicGraph.setAxiom(pos, true)
  }

  def interpretDef(definition: Definition, context: Context): Context =
    definition match {
      case Let(name, vars, bodyOpt) => {
        val Context(logicGraph, stringToExpr) = context
        bodyOpt match {
          case None => {
            val pos = logicGraph.getFreshSymbol
            Context(logicGraph, stringToExpr + (name -> pos))
          }
          case Some(expr) => {
            val pos = interpretExpr(expr, context, vars)
            Context(logicGraph, context.stringToExpr + (name -> pos))
          }
        }
      }
    }

  def contextToLogicState(ctx: Context): LogicState = {
    val stringToExpr = ctx.stringToExpr.filterKeys(!_.startsWith("__"))
    val exprToString = stringToExpr.map(_.swap).toMap
    val printer = Printer(exprToString)
    val solver = new Solver(printer)
    LogicState(ctx.logicGraph, printer, solver)
  }

  protected def apply(program: Program)(ctxt: UtilContext): LogicState = {
    val Program(lets, axioms) = program
    val initContext = Context.init

    val context = lets.foldLeft(initContext) { case (ctx, let) =>
      interpretDef(let, ctx)
    }

    val s2e = context.stringToExpr
    val newContext = axioms.foldLeft(context) {
      case (ctx, axiom) => {
        interpretAxioms(axiom, Context(ctx.logicGraph, s2e))
        Context(ctx.logicGraph, s2e)
      }
    }

    contextToLogicState(newContext)
  }
}
