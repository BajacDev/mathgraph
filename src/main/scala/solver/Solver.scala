package mathgraph.solver

import io.AnsiColor._
import mathgraph.corelogic._
import mathgraph.corelogic.ExprContainer._
import mathgraph.util.Pipe._
import mathgraph.printer._
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

class Solver(val printer: Printer, val suppressPrint: Boolean = false) {

  var stats: Stats = MutMap.empty[Context, MutSet[Int]]
  var arities: Arities = MutMap.empty[Int, Int]

  var size: Int = 0

  // true expr of the form forall a...z. A -> B or A <-> B or A && B or A || B or ~A
  var logicExpr = MutSet.empty[Int]
  // true expr of the form forall something with no imply
  var forallExpr = MutSet.empty[Int]
  var existsExpr = MutSet.empty[Int] // false expr of the form forall something
  var trueGlobalExpr = MutSet.empty[Int] // true expr with no forall as root
  var falseGlobalExpr = MutSet.empty[Int] // false expr with no forall as root

  var noProgress = 0
  val maxAttempts = 100

  def saturation(implicit lg: LogicGraph): Unit = {

    // todo
    if (lg.isAbsurd) ()
    else {
      val formerSize = lg.size

      lg.selectFixer() match {

        case Some(fixerPos) => {
          val selectedFixer = printer.toString(lg, fixerPos)

          update(lg)
          applyInferences(fixerPos)
          evaluateAllImply

          if (formerSize == lg.size) {
            noProgress += 1
            if (noProgress >= maxAttempts) {
              display(s"${RED}No progress. Stopping saturation${RESET}")
              ()
            } else {
              display(
                s"${YELLOW}No Progress. ${maxAttempts - noProgress} attempts left:\t ${selectedFixer}${RESET}"
              )
              saturation(lg)
            }
          } else if (lg.size > MAX_LOGICGRAPH_SIZE) {
            display(s"${RED}Max size reached. Stopping saturation${RESET}")
            ()
          } else {
            noProgress = 0
            display(
              s"${GREEN}Progress made after selecting \t${selectedFixer}${RESET}"
            )
            saturation(lg)
          }
        }
        case _ => ()
      }

      // fixLetSym
      // fixLogicExpr
      // fixForallExpr
      // evaluateAllImply
      // if (formerSize == lg.size) ()
      // else if (lg.size > MAX_LOGICGRAPH_SIZE) ()
      // else saturation(lg)
    }
  }

  def display(str: String): Unit =
    if (!suppressPrint) {
      println(str)
    } else {
      ()
    }

  def applyOnTrueGlobal(pos: Int)(implicit lg: LogicGraph) {
    val argsList = findArgsAndFixIfIn(pos)
    for ((forallPos, args) <- argsList) {
      fixArgsOnPos(forallPos, args)
    }
  }

  def applyOnFalseGlobal(pos: Int)(implicit lg: LogicGraph) {
    val argsList = getPossiblePattern(pos, forallExpr.toList).headOption
    for ((forallPos, args) <- argsList) {
      fixArgsOnPos(forallPos, args)
    }
  }

  def applyOnLogicExpr(pos: Int)(implicit lg: LogicGraph) {
    val argsList = findFixersForForall(pos)
    for (args <- argsList) {
      fixArgsOnPos(pos, args)
    }
  }

  def applyOnExistsExpr(pos: Int)(implicit lg: LogicGraph) {
    lg.fixLetSymbol(pos)
  }

  def applyInferences(expr: Int)(implicit lg: LogicGraph) {

    if (trueGlobalExpr.contains(expr)) {
      applyOnTrueGlobal(expr)

    } else if (falseGlobalExpr.contains(expr)) {
      applyOnFalseGlobal(expr)

    } else if (logicExpr.contains(expr)) {
      applyOnLogicExpr(expr)

    } else if (existsExpr.contains(expr)) {
      applyOnExistsExpr(expr)
    }
    // else if(forallExpr.contains(expr)) {
    //
    // }
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
      val argsList = findFixersForForall(pos)
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

  trait ImplyTree
  object Error extends ImplyTree
  case class End(pos: Int) extends ImplyTree
  case class Inside(pos: Int, left: ImplyTree, right: ImplyTree)
      extends ImplyTree
  case class CannotRepresent(left: ImplyTree, right: ImplyTree)
      extends ImplyTree
  object FalseEnd extends ImplyTree

  object NodeWithChilds {
    def unapply(tree: ImplyTree): Option[(ImplyTree, ImplyTree)] = tree match {
      case Inside(_, a, b)       => Some((a, b))
      case CannotRepresent(a, b) => Some((a, b))
      case _                     => None
    }
  }

  object NodeWithPos {
    def unapply(tree: ImplyTree): Option[Int] = tree match {
      case Inside(pos, a, b) => Some(pos)
      case End(pos)          => Some(pos)
      case _                 => None
    }
  }

  case class ExtendedArgs(
      args: Map[Int, Int],
      opt: Option[(Seq[Int], ExtendedArgs)] = None
  ) {
    lazy val depth: Int = 1 + opt.map(_._2.depth).getOrElse(0)

    def previous(id: Int): Option[(Int, ExtendedArgs)] = opt.flatMap {
      case (seq, prevExtArg) => {
        val i = id - args.size
        if (i >= 0 && i < seq.length) Some(seq(i), prevExtArg)
        else None
      }
    }
  }

  def buildImplyTree(pos: Int, extArgs: ExtendedArgs)(implicit
      lg: LogicGraph
  ): ImplyTree = {
    val args = extArgs.args
    pos match {
      case HeadTail(Symbol(id), Seq(a, b))
          if args.contains(id) && args(id) == ImplySymbol =>
        val left = buildImplyTree(a, extArgs)
        val right = buildImplyTree(b, extArgs)
        if (extArgs.depth > 1) CannotRepresent(left, right)
        else Inside(pos, left, right)

      case HeadTail(Symbol(id), seq) if args.contains(id) =>
        args(id) match {
          case Forall(inside, newArgs) =>
            val newArgsMap = argsToMap(newArgs)
            val newExtendedArg = ExtendedArgs(newArgsMap, Some((seq, extArgs)))
            buildImplyTree(inside, newExtendedArg)

          case FalseSymbol if seq.isEmpty => FalseEnd
          case _                          => if (extArgs.depth > 1) Error else End(pos)
        }

      case Symbol(id) if extArgs.depth > 1 =>
        extArgs.previous(id) match {
          case None                       => Error
          case Some((newPos, newExtArgs)) => buildImplyTree(newPos, newExtArgs)
        }

      // todo instead of Error, cut a part of the tree until it can be expressed as an End()
      // Error can run agains satisfiability

      case _ => if (extArgs.depth > 1) Error else End(pos)
    }
  }

  def implyTreeHasError(implyTree: ImplyTree): Boolean = implyTree match {
    case Error                => true
    case NodeWithChilds(a, b) => implyTreeHasError(a) || implyTreeHasError(b)
    case _                    => false
  }

  def findFixersForForall(
      orig: Int
  )(implicit lg: LogicGraph): List[Map[Int, Int]] = {

    def secondOrderHasTruth(
        implyTree: ImplyTree,
        initialNumArgs: Int,
        args: Map[Int, Int]
    ): Boolean = implyTree match {
      case End(pos) =>
        !(isSecondOrder(pos, initialNumArgs) && getMatchPattern(
          pos,
          args,
          trueGlobalExpr ++ falseGlobalExpr
        ).isEmpty)
      case NodeWithChilds(a, b) =>
        secondOrderHasTruth(a, initialNumArgs, args) && secondOrderHasTruth(
          b,
          initialNumArgs,
          args
        )
      case FalseEnd => true
    }

    def searchTrue(
        implyTree: ImplyTree,
        args: Map[Int, Int]
    ): List[Map[Int, Int]] = {
      val newArgs = implyTree match {
        case NodeWithPos(pos) => getMatchPattern(pos, args, trueGlobalExpr)
        case _                => List()
      }

      implyTree match {
        case NodeWithChilds(a, b) =>
          searchFalse(a, args) ++
            searchFalse(a, args).flatMap(searchFalse(b, _)) ++
            searchFalse(a, args).flatMap(searchTrue(b, _)) ++
            searchTrue(a, args).flatMap(searchTrue(b, _)) ++
            newArgs
        case _ => newArgs
      }
    }

    def searchFalse(
        implyTree: ImplyTree,
        args: Map[Int, Int]
    ): List[Map[Int, Int]] = {
      val newArgs = implyTree match {
        case NodeWithPos(pos) => getMatchPattern(pos, args, falseGlobalExpr)
        case FalseEnd         => List(args)
        case _                => List()
      }

      implyTree match {
        case NodeWithChilds(a, b) =>
          searchTrue(a, args).flatMap(searchFalse(b, _)) ++ newArgs
        case _ => newArgs
      }
    }

    def insideIsFalse(
        implyTree: ImplyTree,
        args: Map[Int, Int]
    ): List[Map[Int, Int]] = implyTree match {
      case NodeWithChilds(a, b) =>
        val newArgsA = insideIsTrue(a, args)
        val newArgsB = insideIsFalse(b, args)
        val both = newArgsA.flatMap(insideIsFalse(b, _))
        newArgsA ++ newArgsB ++ both
      // todo args duplication. use sets
      case _ => List(args)
    }

    def insideIsTrue(
        implyTree: ImplyTree,
        args: Map[Int, Int]
    ): List[Map[Int, Int]] = implyTree match {
      case NodeWithChilds(a, b) =>
        val newArgsA = searchTrue(a, args).flatMap(insideIsTrue(b, _))
        val newArgsB = searchFalse(b, args).flatMap(insideIsFalse(a, _))
        val newArgsAbsurd = searchTrue(a, args).flatMap(searchFalse(b, _))

        newArgsA ++ newArgsB ++ newArgsAbsurd
      // todo args duplication. use sets
      case _ => List(args)
    }

    orig match {
      case Forall(inside, args) => {
        val maxArgs = lg.countSymbols(inside)
        val argsMap = argsToMap(args)
        val implyTree = buildImplyTree(inside, ExtendedArgs(argsMap))
        insideIsTrue(implyTree, argsMap)
          .filter(_.size == maxArgs)
          .filter(secondOrderHasTruth(implyTree, args.size, _))
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
    orig match {
      case Forall(inside, args) =>
        val argsMap = argsToMap(args)
        val implyTree = buildImplyTree(inside, ExtendedArgs(argsMap))
        val containsImply = implyTree match {
          case End(_) => false
          case _      => true
        }
        containsImply && !implyTreeHasError(implyTree)
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
