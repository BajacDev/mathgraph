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

  // true expr of the form forall a...z. A -> B or A <-> B or A && B or A || B or ~A
  var logicExpr = MutSet.empty[Int]
  // true expr of the form forall something with no imply
  var forallExpr = MutSet.empty[Int]
  var trueGlobalExpr = MutSet.empty[Int] // true expr with no forall as root
  var falseGlobalExpr = MutSet.empty[Int] // false expr with no forall as root

  def saturation(implicit lg: LogicGraph): Unit = {

    // todo
    if (lg.isAbsurd) ()
    else {
      val formerSize = lg.size
      fixLogicExpr
      fixForallExpr
      disjonction
      fixLetSym
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
    val fa = (forallExpr ++ logicExpr)
    for (expr <- fa) {
      expr match {
        case Forall(inside, args) => 
          val remain = lg.countSymbols(inside) - args.length
          var pos = expr
          for (e <- 0 until remain) {
            pos = lg.fixLetSymbol(pos)
          }
      }
    }
  }

  def fixLogicExpr()(implicit lg: LogicGraph): Unit = {
    update(lg)
    val fa = (forallExpr ++ logicExpr)
    for (pos <- fa) {
      val argsList = findFixersForForall(pos)
      for (args <- argsList) {
        fixArgsOnPos(pos, args)
      }
    }
  }

  def fixArgsOnPos(orig: Int, argsMap: Map[Int, Int])(implicit
      lg: LogicGraph
  ): Unit = orig match {
    case Forall(_, args) =>
      var pos = orig
      for (i <- args.length until argsMap.size) {
        pos = lg.fixAndSimpl(pos, argsMap(i))
      }
    case _ => ()
  }

  def disjonction()(implicit lg: LogicGraph): Unit = {
    for (expr <- trueGlobalExpr) {
      expr match {
        case HeadTail(ImplySymbol, Seq(a, b)) => {
          lg.disjonction(a)
          lg.disjonction(b)
          // todo more
        }
        case _ => ()
      }
    }
  }
  
  def findFixersForForall(
      orig: Int
  )(implicit lg: LogicGraph): List[Map[Int, Int]] = {

    def secondOrderHasTruth(
        pos: Int,
        initialNumArgs: Int,
        args: Map[Int, Int]
    ): Boolean = pos match {
      case HeadTail(Symbol(imp), Seq(a, b)) if args.lift(imp) == Some(ImplySymbol) => 
        secondOrderHasTruth(a, initialNumArgs, args) && 
        secondOrderHasTruth(b, initialNumArgs, args)
      case _ => 
        !isSecondOrder(pos, initialNumArgs) || 
        !getMatchPattern(pos, args, trueGlobalExpr ++ falseGlobalExpr).isEmpty
    }

    def searchTrue(
        pos: Int,
        args: Map[Int, Int]
    ): List[Map[Int, Int]] = {
      val newArgs = getMatchPattern(pos, args, trueGlobalExpr)

      pos match {
        case HeadTail(Symbol(imp), Seq(a, b)) if args.lift(imp) == Some(ImplySymbol) => 
          searchFalse(a, args) ++
            searchFalse(a, args).flatMap(searchFalse(b, _)) ++
            searchFalse(a, args).flatMap(searchTrue(b, _)) ++
            searchTrue(a, args).flatMap(searchTrue(b, _)) ++
            newArgs
        case _ => newArgs
      }
    }

    def searchFalse(
        pos: Int,
        args: Map[Int, Int]
    ): List[Map[Int, Int]] = {
      val newArgs = getMatchPattern(pos, args, falseGlobalExpr)

      pos match {
        case HeadTail(imp, Seq(a, b)) if args.lift(imp) == Some(ImplySymbol) => 
          searchTrue(a, args).flatMap(searchFalse(b, _)) ++ newArgs
        case _ => newArgs
      }
    }

    def changeMap(outsideArgs: Map[Int, Int], seq: Seq[Int], len: Int): Option[Map[Int, Int]] = {
      var result: Map[Int, Int] = Map()
      for ((arg, i) <- seq.zipWithIndex) {
        arg match {
          case Symbol(id) => 
            if (outsideArgs.contains(i + len)) result = result + (id -> outsideArgs(i + len))
            else return None
          case _ => ()
        }
      }
      Some(result)
    }

    def insideIsFalse(
        pos: Int,
        args: Map[Int, Int]
    ): List[Map[Int, Int]] = {
      val r = pos match {
        case HeadTail(Symbol(imp), Seq(a, b)) if args.lift(imp) == Some(ImplySymbol) => 
          val newArgsA = insideIsTrue(a, args)
          val newArgsB = insideIsFalse(b, args)
          val both = newArgsA.flatMap(insideIsFalse(b, _))
          newArgsA ++ newArgsB ++ both


        case HeadTail(Symbol(headId), tail) => args.lift(headId)  match {
          case Some(Forall(inside, args2)) => 
            val outsideArgs = insideIsFalse(inside, argsToMap(args2))
            // todo args ++ not the best choice
            outsideArgs.flatMap(changeMap(_, tail, args2.length)).map(_ ++ args)
          case _ => List(args)
        }
        // todo args duplication. use sets
        case _ => List(args)
      }

      r ++ searchTrue(pos, args)
    }

    def insideIsTrue(
        pos: Int,
        args: Map[Int, Int]
    ): List[Map[Int, Int]] = {
      val r = pos match {
        case HeadTail(Symbol(imp), Seq(a, b)) if args.lift(imp) == Some(ImplySymbol) => 
          val newArgsA = searchTrue(a, args).flatMap(insideIsTrue(b, _))
          val newArgsB = searchFalse(b, args).flatMap(insideIsFalse(a, _))
          val newArgsAbsurd = searchTrue(a, args).flatMap(searchFalse(b, _))

          newArgsA ++ newArgsB ++ newArgsAbsurd

        case HeadTail(Symbol(headId), tail) => args.lift(headId)  match {
          case Some(Forall(inside, args2)) => 
            val outsideArgs = insideIsTrue(inside, argsToMap(args2))
            // todo args ++ not the best choice
            outsideArgs.flatMap(changeMap(_, tail, args2.length)).map(_ ++ args)
          case _ => List(args)
        }
        // todo args duplication. use sets
        case _ => List(args)
      }

      r ++ searchFalse(pos, args)
    }

    orig match {
      case Forall(inside, args) => {
        val maxArgs = lg.countSymbols(inside)
        val argsMap = argsToMap(args)
        if (lg.isTruth(orig, true)) {
          val r = insideIsTrue(inside, argsMap)
          .filter(_.size == maxArgs)
          .filter(secondOrderHasTruth(inside, args.size, _))
          r
        }
        else if (lg.isTruth(orig, false)) {
          val r = insideIsFalse(inside, argsMap)
          .filter(_.size == maxArgs)
          .filter(secondOrderHasTruth(inside, args.size, _))
          r
        }
        else List()
      }
      case _ => List()
    }
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

  // =============================
  // filter and sort expressions
  // =============================

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
        if (lg.isFixable(expr))
          if (isLogicExpr(expr)) logicExpr += expr
          else forallExpr += expr
        else trueGlobalExpr += expr
      }
    }

    for (expr <- exprSet.filter(lg.isTruth(_, false))) {
      if (!existsFalseFixer(expr)) {
        if (lg.isFixable(expr)) 
          if (isLogicExpr(expr)) logicExpr += expr
          else forallExpr += expr
        else falseGlobalExpr += expr
      }
    }

    size = lg.size
  }

  def existsFalseFixer(pos: Int)(implicit lg: LogicGraph): Boolean = {
    lg.getImplies(pos)
      .exists(other => lg.isTruth(other, false) && lg.isFix(pos, other))
  }

  def existsTrueNext(pos: Int)(implicit lg: LogicGraph): Boolean = {
    lg.getIsImpliedBy(pos)
      .exists(other => lg.isTruth(other, true) && lg.isFix(other, pos))
  }

  def isLogicExpr(orig: Int)(implicit lg: LogicGraph): Boolean = {
    orig match {
      case Forall(inside, args) => inside match {
        case HeadTail(imp, Seq(a, b)) => args.lift(imp) == Some(ImplySymbol)
        case _ => false
      }
      case _ => false
    }
  }
  // =================================================
  // utils functions to find expressions according to patterns
  // =================================================

  def matchPattern(
      pattern: Int,
      idToPos: Map[Int, Int],
      pos: Int
  )(implicit lg: LogicGraph): Option[Map[Int, Int]] = {
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

  // ========================================================================
  //  utils
  // ========================================================================

  def isSecondOrder(pos: Int, numArgs: Int)(implicit lg: LogicGraph): Boolean =
    pos match {
      case HeadTail(Symbol(id), args) if !args.isEmpty =>
        id >= numArgs || args.exists(isSecondOrder(_, numArgs))
      case _ => false
    }

  def argsToMap(args: Seq[Int]): Map[Int, Int] = args.zipWithIndex.map {
    case (arg, idx) => (idx -> arg)
  }.toMap

}
