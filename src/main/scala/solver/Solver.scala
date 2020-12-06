package mathgraph.solver

import mathgraph.corelogic._
import mathgraph.corelogic.ExprContainer._
import mathgraph.util.Pipe._
import mathgraph.solver.Solver._
import scala.collection.mutable.{Map => MutMap, Set => MutSet, ListBuffer}
import scala.math._

object Solver {

  val MAX_LOGICGRAPH_SIZE: Int = 1000

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

  var logicExpr =
    MutSet
      .empty[Int] // true expr of the form forall a...z. A -> B or A <-> B or A && B or A || B or ~A
  var forallExpr =
    MutSet.empty[Int] // true expr of the form forall something with no imply
  var existsExpr = MutSet.empty[Int] // false expr of the form forall something
  var trueGlobalExpr = MutSet.empty[Int] // true expr with no forall as root
  var falseGlobalExpr = MutSet.empty[Int] // false expr with no forall as root

  var AndSymbol = -1
  var OrSymbol = -1
  var IffSymbol = -1

  def retriveLogicSymbols(stringToExpr: Map[String, Int]): Unit = {
    stringToExpr.get("&").foreach(AndSymbol = _)
    stringToExpr.get("|").foreach(OrSymbol = _)
    stringToExpr.get("<=>").foreach(IffSymbol = _)
  }

  def saturation(implicit lg: LogicGraph): Unit = {

    // todo
    if (lg.isAbsurd) ()
    else {
      val formerSize = lg.size
      fixLetSym
      fixLogicExpr
      fixForallExpr
      evaluateAllImply
      if (formerSize == lg.size) ()
      else if (lg.size > MAX_LOGICGRAPH_SIZE) ()
      else saturation(lg)
    }
  }

  /** fix all expressions with their let symbol
    * for now, I fix every false expression because I did not find
    * proofs where  you have to fix true expression. That can change
    */
  def fixLetSym()(implicit lg: LogicGraph): Unit = {
    update(lg)
    for (pos <- existsExpr) {
      lg.fixLetSymbol(pos)
    }
  }

  def fixLogicExpr()(implicit lg: LogicGraph): Unit = {
    update(lg)
    for (pos <- logicExpr) {
      val argsList = findArgsAndFix(pos)
      for (args <- argsList) {
        fixArgsOnPos(pos, args)
      }
    }
  }

  def evaluateAllImply()(implicit lg: LogicGraph): Unit = {
    for (i <- 0 until lg.size) {
      lg.implyUpInferenceRule(i)
    }
  }

  def fixArgsOnPos(orig: Int, argsMap: Map[Int, Int])(implicit
      lg: LogicGraph
  ): Unit = orig match {
    case Forall(_, args) =>
      var pos = orig
      for (i <- args.length until argsMap.size) {
        pos = lg.fix(pos, argsMap(i))
      }
    case _ => ()
  }

  // general todo
  def findArgsAndFix(
      orig: Int
  )(implicit lg: LogicGraph): ListBuffer[Map[Int, Int]] = {

    val possibleArgs = ListBuffer.empty[Map[Int, Int]]
    var maxArgs = 0

    def addPossible(args: Map[Int, Int]): Unit =
      if (args.size == maxArgs) possibleArgs += args

    def processInsideFalse(pos: Int, args: Map[Int, Int]): Unit = pos match {
      case HeadTail(Symbol(id), seq) if args.contains(id) =>
        if (args(id) == ImplySymbol) {
          // todo
        } else if (args(id) == OrSymbol) {} // todo
        else if (args(id) == AndSymbol) {
          // todo
          addPossible(args)
        } else if (args(id) == IffSymbol) {} // todo
        else addPossible(args)
      case _ => addPossible(args)
    }

    def processInsideTrue(pos: Int, args: Map[Int, Int]): Unit = pos match {
      case HeadTail(Symbol(id), Seq(a, b)) if args.contains(id) =>
        if (args(id) == ImplySymbol) {
          var newArgs = getMatchPattern(a, args, trueGlobalExpr)
          newArgs.foreach(processInsideTrue(b, _))

          newArgs = getMatchPattern(b, args, falseGlobalExpr)
          newArgs.foreach(processInsideFalse(a, _))
        } else if (args(id) == OrSymbol) () // todo
        else if (args(id) == AndSymbol) () // todo
        else if (args(id) == IffSymbol) () // todo
        else addPossible(args)
      case _ => addPossible(args)
    }

    orig match {
      case Forall(inside, args) => {
        val argsMap = args.zipWithIndex.map { case (arg, idx) =>
          (idx -> arg)
        }.toMap
        maxArgs = lg.countSymbols(inside)
        processInsideTrue(inside, argsMap)
      }
      case _ => ()
    }

    possibleArgs
  }

  def fixForallExpr()(implicit lg: LogicGraph): Unit = {
    update(lg)
    for (pos <- trueGlobalExpr) {
      val argsList = findArgsAndFixIfIn(pos)
      for ((forallPos, args) <- argsList) {
        fixArgsOnPos(forallPos, args)
      }
    }

    for (pos <- falseGlobalExpr) {
      val argsList = getPossiblePattern(pos, forallExpr.toList).headOption
      for ((forallPos, args) <- argsList) {
        fixArgsOnPos(forallPos, args)
      }
    }

    // todo false implies
  }

  // general todo
  def findArgsAndFixIfIn(
      orig: Int
  )(implicit lg: LogicGraph): Option[(Int, Map[Int, Int])] = {

    // todo: not list
    var result: List[(Int, Map[Int, Int])] = List()

    def processFalseImply(pos: Int): Unit = pos match {
      case HeadTail(ImplySymbol, Seq(a, b)) => {
        if (lg.getTruthOf(a).isEmpty) {
          processTrueImply(a)
          // todo
        }
        if (lg.getTruthOf(b).isEmpty) {
          // todo
        }
      }
      case _ => ()
    }

    def processTrueImply(pos: Int): Unit = {
      pos match {
        case HeadTail(ImplySymbol, Seq(a, b)) => {
          if (lg.getTruthOf(a).isEmpty) {
            // todo
            result = result ++ getPossiblePattern(a, forallExpr.toList)
          }
          if (lg.getTruthOf(b).isEmpty) {
            // todo
            processFalseImply(b)
          }
        }

        case _ => ()
      }
    }

    processTrueImply(orig)

    // todo we only need head
    result.headOption
  }

  def update(implicit
      lg: LogicGraph
  ): Unit = {

    def exprSet = lg.getAllTruth
      // remove occurance of trueSymbol with a tail
      // appears because of forall simplify
      .filter {
        case HeadTail(TrueSymbol, seq) if seq.length > 0 => false
        case _                                           => true
      }

    for (expr <- exprSet.filter(lg.isTruth(_, true))) {
      if (!existsTrueNext(expr)) {
        if (isLogicExpr(expr)) logicExpr += expr
        else if (lg.isFixable(expr)) forallExpr += expr
        else trueGlobalExpr += expr
      }
    }

    for (expr <- exprSet.filter(lg.isTruth(_, false))) {
      if (!existsFalseFixer(expr)) {
        if (lg.isFixable(expr)) existsExpr += expr
        else falseGlobalExpr += expr
      }
    }

    size = lg.size
  }

  def existsFalseFixer(pos: Int)(implicit lg: LogicGraph): Boolean = {
    lg.getImplies(pos)
      .exists(other => lg.isFixOf(other, pos) && lg.isTruth(other, false))
  }

  def existsTrueNext(pos: Int)(implicit lg: LogicGraph): Boolean = pos match {
    case Fixer(next, _) => lg.isTruth(next, true)
    case _              => false
  }

  def isLogicExpr(orig: Int)(implicit lg: LogicGraph): Boolean = {

    def isLogicExprInside(pos: Int, args: Seq[Int]): Boolean = pos match {
      case HeadTail(Symbol(id), Seq(a, b)) if id < args.length =>
        if (args(id) == ImplySymbol) true
        else if (args(id) == OrSymbol) true
        else if (args(id) == AndSymbol) true
        else if (args(id) == IffSymbol) true
        else false
      case _ => false
    }

    orig match {
      case Forall(inside, args) => isLogicExprInside(inside, args)
      case _                    => false
    }
  }
  // =================================================
  // utils functions to find expressions according to patterns
  // =================================================

  def matchPattern(
      pattern: Int,
      idToPos: Map[Int, Int],
      pos: Int
  )(implicit lg: LogicGraph): Option[Map[Int, Int]] =
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

  def getMatchPattern(
      pattern: Int,
      args: Map[Int, Int],
      exprList: MutSet[Int]
  )(implicit lg: LogicGraph): List[Map[Int, Int]] = {
    exprList.toList.flatMap(matchPattern(pattern, args, _))
  }

  def getPossiblePattern(
      orig: Int,
      patterns: List[Int]
  )(implicit lg: LogicGraph): List[(Int, Map[Int, Int])] = {

    def argsToMap(args: Seq[Int]): Map[Int, Int] = args.zipWithIndex.map {
      case (arg, idx) => (idx -> arg)
    }.toMap

    def getInside(pos: Int): Option[(Int, Int, Map[Int, Int])] = pos match {
      case Forall(inside, args) => Some((pos, inside, argsToMap(args)))
      case _                    => None
    }

    patterns.flatMap(getInside).flatMap { case (pos, pattern, args) =>
      matchPattern(pattern, args, orig) match {
        case Some(map) => Some((pos, map))
        case _         => None
      }
    }
  }

}
