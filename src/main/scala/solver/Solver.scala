package mathgraph.solver

import mathgraph.corelogic._
import mathgraph.util.Pipe._

object Solver {

  // context an expr is used
  // head can be a symbol or an expr if the head is a forall
  // eg: ->(A, B)
  // the context of A is Context(->, 0)
  // this does not accept second order logic context yet
  case class Context(head: Int, idArg: Int)

  // todo better name
  type Stats = Map[Context, Set[Int]]

  def saturation(logicGraph: LogicGraph): LogicGraph = {
    if (logicGraph.isAbsurd) logicGraph
    else saturation(fixAll(fixLetSym(logicGraph)))
  }

  def fixLetSym(logicGraph: LogicGraph): LogicGraph = {
    logicGraph
      .getAllTruth(false)
      .filter(logicGraph.isFixable)
      .foldLeft(logicGraph) { case (lg, pos) =>
        lg.applyLetSymbol(pos)._1
      }
  }

  private def fixArgSet(
      logicGraph: LogicGraph,
      pos: Int,
      argSet: Set[Int]
  ): LogicGraph = {
    argSet.foldLeft(logicGraph) { case (lg, arg) =>
      lg.apply(pos, arg)._1
    }
  }

  private def fixPos(
      logicGraph: LogicGraph,
      pos: Int,
      stats: Stats
  ): LogicGraph = {

    val contextSet: Set[Context] = getContexts(logicGraph, pos)
    contextSet.foldLeft(logicGraph) { case (lg, context) =>
      stats.get(context) match {
        case None      => lg
        case Some(set) => fixArgSet(lg, pos, set)
      }
    }
  }

  def fixAll(logicGraph: LogicGraph): LogicGraph = {
    val exprSet = logicGraph.getAllTruth
    val stats = getStats(logicGraph, exprSet)
    exprSet.filter(logicGraph.isFixable).foldLeft(logicGraph) {
      case (lg, pos) => fixPos(lg, pos, stats)
    }
  }

  // duplicated
  private def insertPair(a: Context, b: Int, map: Stats): Stats =
    map get a match {
      case None    => map + (a -> Set(b))
      case Some(s) => (map - a) + (a -> (s + b))
    }

  def getStats(logicGraph: LogicGraph, exprSet: Set[Int]): Stats = {

    def getStatsHeadTail(head: Int, tail: Seq[Int], result: Stats): Stats = {
      tail.zipWithIndex.foldLeft(result) { case (map, (arg, idx)) =>
        tail.foldLeft(insertPair(Context(head, idx), arg, result)) {
          case (map2, pos) => getStatsPos(pos, map2)
        }
      }
    }

    def getStatsPos(pos: Int, result: Stats): Stats = {
      logicGraph.unfoldForall(pos) match {
        case None =>
          logicGraph.getHeadTailInt(pos) |> { case (head, tail) =>
            getStatsHeadTail(head, tail, result)
          }
        case Some((inside, args)) => getStatsHeadTail(inside, args, result)
      }
    }

    exprSet.foldLeft(Map[Context, Set[Int]]()) { case (map, pos) =>
      getStatsPos(pos, map)
    }
  }

  def getContexts(logicGraph: LogicGraph, pos: Int): Set[Context] = {
    def getContextsOutside(p: Int): Context = {
      logicGraph.unfoldForall(p) match {
        case None =>
          logicGraph.getHeadTailInt(p) |> { case (head, tail) =>
            Context(head, tail.length)
          }
        case Some((inside, args)) => Context(inside, args.length)
      }
    }
    val contexts: Set[Context] = logicGraph.unfoldForall(pos) match {
      case Some((inside, args)) =>
        getContextsInside(logicGraph, inside, args.map(getContextsOutside _))
      case _ => Set()
    }
    contexts + getContextsOutside(pos)
  }

  def getContextsInside(
      logicGraph: LogicGraph,
      pos: Int,
      outsideArgs: Seq[Context]
  ): Set[Context] = {
    val idArg = outsideArgs.length
    val (sym, args) = logicGraph.getHeadTail(pos)

    // treat the == case for 2nd order logic
    if (sym.id >= idArg) Set()
    else
      args.zipWithIndex.foldLeft(Set[Context]()) { case (set, (arg, idx)) =>
        set ++ (logicGraph.getExpr(arg) match {

          case Symbol(idSym) if idSym == idArg =>
            Set(
              Context(outsideArgs(sym.id).head, idx + outsideArgs(sym.id).idArg)
            )

          case Apply(_, _) => getContextsInside(logicGraph, arg, outsideArgs)
        })
      }
  }

}
