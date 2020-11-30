package mathgraph.solver

import mathgraph.corelogic._
import mathgraph.corelogic.ExprContainer._
import mathgraph.util.Pipe._
import mathgraph.solver.Solver._
import scala.collection.mutable.{Map => MutMap}
import scala.collection.mutable.{Set => MutSet}
import scala.math._

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
  type Stats = MutMap[Context, MutSet[Int]]
  type Arities = MutMap[Int, Int]

}

class Solver() {

  var stats: Stats = MutMap.empty[Context, MutSet[Int]]
  var arities: Arities = MutMap.empty[Int, Int]
  var size: Int = 0

  def saturation(lg: LogicGraph): LogicGraph = {

    if (lg.isAbsurd) lg
    else {
      val formerSize = lg.size
      val nextLg = fixAll(fixLetSym(lg))
      if (formerSize == nextLg.size) nextLg
      else saturation(nextLg)
    }
  }

  /** fix all expressions with their let symbol
    * for now, I fix every false expression because I did not find
    * proofs where  you have to fix true expression. That can change
    */
  def fixLetSym(logicGraph: LogicGraph): LogicGraph = {
    update(logicGraph)
    logicGraph
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
    update(logicGraph)
    val exprSet = logicGraph.getAllTruth.filter(logicGraph.isFixable)
    exprSet.foldLeft(logicGraph) { case (lg, pos) =>
      fixPos(lg, pos)
    }
  }

  /** fix the expression pos using Contexts in stats
    */
  private def fixPos(
      logicGraph: LogicGraph,
      pos: Int
  ): LogicGraph = {

    val futureArgs = getFutureArgs(logicGraph, pos)
    futureArgs.foldLeft(logicGraph) { case (lg, arg) =>
      lg.fix(pos, arg)._1
    }
  }

  def insertInStats(ctx: Context, pos: Int): Unit = stats get ctx match {
    case Some(set) => set += pos
    case None      => stats += ctx -> MutSet(pos)
  }

  /** return stats from a set of expression
    *
    * eg: expression +(1, 4) will return the map
    * Map(Context('+', 0) -> Set('1'), Context('+', 1) -> Set('4'))
    *
    * eg: expression +(1, f(7)) will return the map
    * Map(Context('+', 0) -> Set('1'), Context('+', 1) -> Set('f(7)'), Context('f', 0) -> Set('7'))
    */
  def update(implicit
      lg: LogicGraph
  ): Unit = {

    def recHeadTail(head: Int, tail: Seq[Int]): Unit = {
      for ((arg, idArg) <- tail.zipWithIndex) {
        insertInStats(Context(head, idArg), arg)
        arities(head) = max(arities.getOrElse(head, 0), tail.length)
        recPos(arg)
      }
    }

    // return stats from one expression
    def recPos(pos: Int): Unit = {
      pos match {
        case Forall(_, _)         => ()
        case HeadTail(head, tail) => recHeadTail(head, tail)
      }
    }

    val exprSet = lg.getAllTruth
      .filter(_ >= size)
      // remove occurance of trueSymbol with a tail
      // appears because of forall simplify
      .filter {
        case HeadTail(TrueSymbol, _) => false
        case _                       => true
      }

    for (expr <- exprSet) {
      recPos(expr)
    }

    size = lg.size
  }

  /** Return the context of the argument to be fixed
    * eg: if pos is {0(1,2)}(+(a)), then it return the context of the first arg
    * inside the forall and outside
    * here returns Set(Context({0(1,2)}, 1), Context(+, 2))
    */
  def getContexts(implicit
      logicGraph: LogicGraph,
      pos: Int
  ): MutSet[Context] = {
    pos match {
      case Forall(inside, args) =>
        getContextsInside(logicGraph, inside, args)
      case _ => MutSet.empty[Context]
    }
  }

  def getContextsInside(implicit
      logicGraph: LogicGraph,
      orig: Int,
      outsideArgs: Seq[Int]
  ): MutSet[Context] = {
    var result = MutSet.empty[Context]
    val idArg = outsideArgs.length

    def contextInsideRec(pos: Int): Unit = {
      pos match {
        case HeadTail(Symbol(id), args) if id < idArg => ctx(id, args)
        case _                                        => ()
      }

      pos match {
        case HeadTail(_, args) => {
          for (arg <- args) contextInsideRec(arg)
        }
      }
    }

    def ctx(id: Int, args: Seq[Int]): Unit = {
      val (head, len) = outsideArgs(id) match {
        case HeadTail(h, a) => (h, a.length)
      }
      for ((arg, idx) <- args.zipWithIndex) {
        arg match {

          // eg: {0(1,2)}(+(a))
          // when we simplify this expression with arg number 1 and 2 fixed, we obtain
          // +(a, arg1). So the arguement number 1 in this context: ('+', 2)
          case Symbol(idSym) if idSym == idArg => {
            result += Context(head, len + idx)
          }

          case _ => ()
        }
      }
    }

    contextInsideRec(orig)
    result
  }

  def getArity(implicit logicGraph: LogicGraph, pos: Int): Int =
    pos match {
      case Forall(inside, args) =>
        getArityInside(logicGraph, inside, args.length)
      case _ => 0
    }

  def getArityInside(implicit lg: LogicGraph, pos: Int, idArg: Int): Int = {
    def tailMax(tail: Seq[Int]): Int =
      (tail.map(getArityInside(lg, _, idArg)) :+ 0).max

    pos match {
      case HeadTail(Symbol(id), tail) if id == idArg => {
        max(tail.length, tailMax(tail))
      }
      case HeadTail(_, tail) => tailMax(tail)
      case _                 => 0
    }
  }

  def getFutureArgs(implicit logicGraph: LogicGraph, pos: Int): MutSet[Int] = {
    val contexts = getContexts(logicGraph, pos)
    val arityInside = getArity(logicGraph, pos)
    var result = MutSet.empty[Int]

    for (ctx <- contexts) {
      result ++= stats.getOrElse(ctx, MutSet.empty[Int])
    }

    result = result.filter(trueForallImplyMustBeUsed(logicGraph, pos, _))

    def remainingArity(pos: Int) = pos match {
      case HeadTail(head, tail) => arities.getOrElse(head, 0) - tail.length
    }

    if (arityInside > 0) result.filter(remainingArity(_) >= arityInside)
    else result.filter(remainingArity(_) == 0)
  }

  def fixGetThruth(implicit
      lg: LogicGraph,
      orig: Int,
      args: Seq[Int]
  ): Option[Boolean] = {
    def fixRec(pos: Int): Option[Int] = pos match {
      case Symbol(id) => Some(args(id))
      case Fixer(next, arg) => {
        val nextOpt: Option[Int] = fixRec(next)
        val argOpt: Option[Int] = fixRec(arg)
        (nextOpt, argOpt) match {
          case (Some(newNext), Some(newArg)) => lg.fixerToPos(newNext, newArg)
          case _                             => None
        }
      }
    }

    fixRec(orig).flatMap(lg.getTruthOf)
  }

  def trueForallImplyMustBeUsed(implicit
      lg: LogicGraph,
      orig: Int,
      newArg: Int
  ): Boolean = {

    def isFixable(pos: Int, args: Seq[Int]): Boolean =
      lg.countSymbols(pos) <= args.length

    def processInside(inside: Int, args: Seq[Int]): Boolean =
      inside match {
        case HeadTail(Symbol(id), Seq(a, b)) if args(id) == ImplySymbol =>
          if (isFixable(a, args))
            if (fixGetThruth(lg, a, args) == Some(true)) processInside(b, args)
            else if (falseMatchPatternExists(lg, b, args)) true
            else false
          else if (isFixable(b, args))
            if (fixGetThruth(lg, b, args) == Some(false)) true
            else if (trueMatchPatternExists(lg, a, args)) true
            else false
          else true

        case _ => true
      }

    (orig, lg.getTruthOf(orig)) match {
      case (Forall(inside, args), Some(true)) =>
        processInside(inside, args :+ newArg)
      case _ => true
    }
  }

  def trueMatchPatternExists(implicit
      lg: LogicGraph,
      orig: Int,
      args: Seq[Int]
  ): Boolean = {
    getMatchPattern(lg, orig, args, lg.getAllTruth(true))
  }

  def falseMatchPatternExists(implicit
      lg: LogicGraph,
      orig: Int,
      args: Seq[Int]
  ): Boolean = {
    getMatchPattern(lg, orig, args, lg.getAllTruth(false))
  }

  def getMatchPattern(implicit
      lg: LogicGraph,
      orig: Int,
      args: Seq[Int],
      exprSet: Set[Int]
  ): Boolean = {
    def matchPattern(
        pattern: Int,
        idToPos: Map[Int, Int],
        pos: Int
    ): Option[Map[Int, Int]] =
      (pattern, pos) match {
        case (Fixer(nextP, argP), Fixer(next, arg)) => {
          matchPattern(nextP, idToPos, next)
            .flatMap(matchPattern(argP, _, arg))
        }
        case (Symbol(idP), expr) =>
          idToPos.get(idP) match {
            case None                         => Some(idToPos + (idP -> expr))
            case Some(exprP) if exprP == expr => Some(idToPos)
            case Some(exprP) if exprP != expr => None
          }
        case _ => None
      }

    val map = args.zipWithIndex.map { case (arg, idx) => (idx -> arg) }.toMap
    exprSet.exists(expr =>
      matchPattern(orig, map, expr) match {
        case None    => false
        case Some(m) => true
      }
    )
  }

}
