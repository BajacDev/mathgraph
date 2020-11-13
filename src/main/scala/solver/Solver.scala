package mathgraph.solver

import mathgraph.corelogic._
import mathgraph.util.Pipe._

object Solver {

  /** context an expr is used
    * head can be a symbol or an expr if the head is a forall
    *
    * eg: ->(A, B)
    * the context of A is Context(->, 0)
    *
    * when there is a forall, we take the arg number 0 as an head
    * that make stats more precise
    *
    * this does not accept second order logic context yet
    */
  case class Context(head: Int, idArg: Int)

  // todo better name
  type Stats = Map[Context, Set[Int]]

  def saturation(logicGraph: LogicGraph): LogicGraph = {
    if (logicGraph.isAbsurd) logicGraph
    else saturation(fixAll(fixLetSym(logicGraph)))
  }

  /** fix all expressions with their let symbol
    * for now, I fix every false expression because I did not find
    * proofs where  you have to fix true expression. That can change
    */
  def fixLetSym(logicGraph: LogicGraph): LogicGraph = {
    logicGraph
      .getAllTruth(false)
      .filter(logicGraph.isFixable)
      .foldLeft(logicGraph) { case (lg, pos) =>
        lg.applyLetSymbol(pos)._1
      }
  }

  def fixAll(logicGraph: LogicGraph): LogicGraph = {
    val exprSet = logicGraph.getAllTruth
    val stats = getStats(logicGraph, exprSet)
    exprSet.filter(logicGraph.isFixable).foldLeft(logicGraph) {
      case (lg, pos) => fixPos(lg, pos, stats)
    }
  }

  /** fix the expression pos using Contexts in stats
    */
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

  private def fixArgSet(
      logicGraph: LogicGraph,
      pos: Int,
      argSet: Set[Int]
  ): LogicGraph = {
    argSet.foldLeft(logicGraph) { case (lg, arg) =>
      lg.apply(pos, arg)._1
    }
  }

  // duplicated
  private def insertPair(a: Context, b: Int, map: Stats): Stats =
    map get a match {
      case None    => map + (a -> Set(b))
      case Some(s) => (map - a) + (a -> (s + b))
    }

  /** return stats from a set of expression
    *
    * eg: expression +(1, 4) will return the map
    * Map(Context('+', 0) -> Set('1'), Context('+', 1) -> Set('4'))
    *
    * eg: expression +(1, f(7)) will return the map
    * Map(Context('+', 0) -> Set('1'), Context('+', 1) -> Set('f(7)'), Context('f', 0) -> Set('7'))
    */
  def getStats(logicGraph: LogicGraph, exprSet: Set[Int]): Stats = {

    def getStatsHeadTail(head: Int, tail: Seq[Int], result: Stats): Stats = {
      tail.zipWithIndex.foldLeft(result) { case (map, (arg, idx)) =>
        tail.foldLeft(insertPair(Context(head, idx), arg, result)) {
          case (map2, pos) => getStatsPos(pos, map2)
        }
      }
    }

    // return stats from one expression
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

  /** Return the context of the argument to be fixed
    * eg: if pos is {0(1,2)}(+(a)), then it return the context of the first arg
    * inside the forall and outside
    * here returns Set(Context({0(1,2)}, 1), Context(+, 2))
    */
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
      // use Context to store (head, tail.size)
      outsideArgs: Seq[Context]
  ): Set[Context] = {
    val idArg = outsideArgs.length
    val (sym, args) = logicGraph.getHeadTail(pos)

    // if we want to process 2nd order logic, we must take == case into account
    if (sym.id >= idArg) Set()
    else
      args.zipWithIndex.foldLeft(Set[Context]()) { case (set, (arg, idx)) =>
        set ++ (logicGraph.getExpr(arg) match {

          // eg: {0(1,2)}(+(a))
          // when we simplify this expression with arg number 1 and 2 fixed, we obtain
          // +(a, arg1). So the arguement number 1 in this context: ('+', 2)
          case Symbol(idSym) if idSym == idArg =>
            Set(
              Context(outsideArgs(sym.id).head, idx + outsideArgs(sym.id).idArg)
            )

          case Apply(_, _) => getContextsInside(logicGraph, arg, outsideArgs)
        })
      }
  }

}
