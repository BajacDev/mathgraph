package mathgraph.solver

import mathgraph.corelogic._
import mathgraph.util.Pipe._
import mathgraph.solver.Solver._

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
  type Arities = Map[Context, Set[Int]]

}

case class Solver(stats: Stats = Map[Context, Set[Int]](), size: Int = 0) {

  def saturation(lg: LogicGraph): (LogicGraph, Solver) = {
    if (lg.isAbsurd) (lg, this)
    else {
      val s = updateSolver(lg)
      val nextLg = s.fixAll(s.fixLetSym(lg))
      if (nextLg.size == lg.size) (nextLg, s)
      else s.saturation(nextLg)
    }
  }

  /** fix all expressions with their let symbol
    * for now, I fix every false expression because I did not find
    * proofs where  you have to fix true expression. That can change
    */
  def fixLetSym(logicGraph: LogicGraph): Unit = {
    val exprSet = logicGraph
      .getAllTruth(false)
      .filter(logicGraph.isFixable)
      .filter(!existsFalseFixer(logicGraph, _))

    for (expr <- exprSet) {
        logicGraph.fixLetSymbol(pos)
    }
  }

  def existsFalseFixer(logicGraph: LogicGraph, pos: Int): Boolean = {
    logicGraph
      .getImplies(pos)
      .exists(other =>
        logicGraph.isFixOf(other, pos) && logicGraph.isTruth(other, false)
      )
  }

  /** fix all expressions using stats * */
  def fixAll(logicGraph: LogicGraph): LogicGraph = {
    val exprSet = logicGraph.getAllTruth
    exprSet.filter(logicGraph.isFixable).foldLeft(logicGraph) {
      case (lg, pos) => fixPos(lg, pos)
    }
  }

  /** fix the expression pos using Contexts in stats
    */
  private def fixPos(
      logicGraph: LogicGraph,
      pos: Int
  ): LogicGraph = {

    val contextSet: Set[Context] = getContexts(logicGraph, pos)
    for (context <- contextSet) {
      stats.get(context) match {
        case None      => ()
        case Some(set) => fixArgSet(logicGraph, pos, set)
      }
    }
  }

  private def fixArgSet(
      logicGraph: LogicGraph,
      pos: Int,
      argSet: Set[Int]
  ): Unit = {
    for (arg <- argSet) {
      logicGraph.fix(pos, arg)
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
  def updateSolver(implicit
      lg: LogicGraph
  ): Solver = {

    def recHeadTail(head: Int, tail: Seq[Int], result: Stats): Stats = {
      tail.zipWithIndex.foldLeft(result) { case (map, (arg, idx)) =>
        val newStats = insertPair(Context(head, idx), arg, map)
        recPos(arg, newStats)
      }
    }

    // return stats from one expression
    def recPos(pos: Int, result: Stats): Stats = {
      pos match {
        case Forall(_, _)         => result
        case HeadTail(head, tail) => recHeadTail(head, tail, result)
      }
    }

    val exprSet = lg.getAllTruth.filter(_ >= size)

    val newStats = exprSet.foldLeft(stats) { case (map, pos) =>
      recPos(pos, map)
    }

    Solver(newStats, lg.size)
  }

  /** Return the context of the argument to be fixed
    * eg: if pos is {0(1,2)}(+(a)), then it return the context of the first arg
    * inside the forall and outside
    * here returns Set(Context({0(1,2)}, 1), Context(+, 2))
    */
  def getContexts(implicit logicGraph: LogicGraph, pos: Int): Set[Context] = {
    pos match {
      case Forall(inside, args) =>
        getContextsInside(logicGraph, inside, args)
      case _ => Set()
    }
  }

  def getContextsInside(implicit
      logicGraph: LogicGraph,
      orig: Int,
      outsideArgs: Seq[Int]
  ): Set[Context] = {
    val idArg = outsideArgs.length

    def contextInsideRec(pos: Int): Set[Context] = {
      val result: Set[Context] = pos match {
        case HeadTail(Symbol(id), _) if id >= idArg => Set()
        case HeadTail(Symbol(id), args)             => ctx(id, args)
      }

      pos match {
        case HeadTail(_, args) =>
          args.foldLeft(result) { case (set, arg) =>
            set ++ contextInsideRec(arg)
          }
      }
    }

    def ctx(id: Int, args: Seq[Int]): Set[Context] = {
      args.zipWithIndex.foldLeft(Set[Context]()) { case (set, (arg, idx)) =>
        arg match {

          // eg: {0(1,2)}(+(a))
          // when we simplify this expression with arg number 1 and 2 fixed, we obtain
          // +(a, arg1). So the arguement number 1 in this context: ('+', 2)
          case Symbol(idSym) if idSym == idArg =>
            outsideArgs(id) match {
              case HeadTail(head, argsOut) =>
                set + Context(head, argsOut.length + idx)
            }

          case _ => set
        }
      }
    }

    contextInsideRec(orig)
  }

}
