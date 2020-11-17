package mathgraph.backend

import mathgraph.frontend._
import mathgraph.repl._
import mathgraph.corelogic.LogicGraph
import mathgraph.frontend.Trees._
import mathgraph.util._
import mathgraph.util.{Context => UtilContext}
import mathgraph.backend.{BackendContext => Context}
import mathgraph.printer._

object ProgToLogicState extends Pipeline[Program, LogicState] {

  def globalIdentifiers(
      expr: Expr,
      allGlobalIds: Set[Identifier]
  ): Set[Identifier] = expr match {
    case Apply(id, args) => {
      val set: Set[Identifier] =
        if (allGlobalIds.contains(id)) Set(id)
        else Set()
      args.foldLeft(set) { case (s, arg) =>
        s ++ globalIdentifiers(arg, allGlobalIds)
      }
    }
  }

  def exprToLogicGraph(
      expr: Expr,
      stringToExpr: Map[Identifier, Int],
      logicGraph: LogicGraph
  ): (LogicGraph, Int) = expr match {
    case Apply(id, args) => {
      val head = (logicGraph, stringToExpr(id))
      args.foldLeft(head) {
        case ((lg, pos), arg) => {
          val (lg2, argPos) = exprToLogicGraph(arg, stringToExpr, lg)
          lg2.apply(pos, argPos)
        }
      }
    }
  }

  def interpretExpr(
      expr: Expr,
      context: Context,
      vars: Seq[Identifier]
  ): (LogicGraph, Int) = {
    val Context(logicGraph, globalstringToExpr) = context
    if (vars.length == 0) {
      exprToLogicGraph(expr, globalstringToExpr, logicGraph)
    } else {
      val globals = globalIdentifiers(expr, globalstringToExpr.keySet).toSeq
      val freeVar = globals ++ vars
      val localStringToExpr = freeVar.zipWithIndex.map { case (id, symbolId) =>
        (id, logicGraph.idToPos(symbolId))
      }.toMap
      val (lg, pos) = exprToLogicGraph(expr, localStringToExpr, logicGraph)
      globals.foldLeft(lg.forall(pos)) { case ((lg2, pos2), arg) =>
        lg2.apply(pos2, globalstringToExpr(arg))
      }
    }
  }

  def interpretAxioms(expr: Expr, context: Context): LogicGraph = {
    val (lg, pos) = interpretExpr(expr, context, Seq())
    lg.setAxiom(pos, true)
  }

  def interpretDef(definition: Def, context: Context): Context =
    definition match {
      case Let(name, vars, bodyOpt) => {
        val Context(logicGraph, stringToExpr) = context
        bodyOpt match {
          case None => {
            val (lg, pos) = logicGraph.getFreshSymbol
            Context(lg, stringToExpr + (name -> pos))
          }
          case Some(expr) => {
            val (lg, pos) = interpretExpr(expr, context, vars)
            Context(lg, context.stringToExpr + (name -> pos))
          }
        }
      }
    }

  def contextToLogicState(ctx: Context): LogicState = {
    val printer = Printer(ctx.logicGraph, ctx.stringToExpr.map(_.swap))
    LogicState(ctx.logicGraph, printer, None)
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
        val newLg = interpretAxioms(axiom, Context(ctx.logicGraph, s2e))
        Context(newLg, s2e)
      }
    }

    contextToLogicState(newContext)
  }
}
