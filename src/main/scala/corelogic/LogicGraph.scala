package mathgraph.corelogic

import mathgraph.util.Pipe._

/** The logic layer manage the truth graph of expressions
  * Notation:
  * -> means the symbol imply (see below)
  * => means imply in the LogicGraph. when A => B, A is linked in the graph to B
  *
  * questions that have risen during the conception:
  *
  * why using '->' as a base Symbol and not 'not', 'or' and 'and'?
  * - I wanted to have the minimum number of symbols. Moreover I wanted the base symbols
  *   to have a strong relationship with the graph we are manipulating. hence `->`
  *   have a instinctive meaning on the graph (see imply infrence rule).
  *   It is not based on any performance requirements
  *
  * why ysing false propagation instead of just true propagation ?
  * - I wanted the minimum number of inference rules. using only true propagation
  *   would have increase the number of inference rule, specialy with
  *   negations of forall (exists). See future proof of test.txt
  */

// -----------------------------------------------------
// Logic Symbols
// -----------------------------------------------------

/** use to build other symbols * */
object DefSymbol extends Symbol(0)

object FalseSymbol extends Symbol(1)
object TrueSymbol extends Symbol(2)

/** implication symbol (eg: a -> b) * */
object ImplySymbol extends Symbol(3)

/** forall symbol * */
object ForallSymbol extends Symbol(4)

object Forall {
  def unapply(pos: Int)(implicit lg: LogicGraph): Option[(Int, Seq[Int])] =
    lg.unfoldForall(pos)
}

object HeadTail {
  def unapply(pos: Int)(implicit lg: LogicGraph): Option[(Int, Seq[Int])] =
    Some(lg.getHeadTailInt(pos))
}

// -----------------------------------
// inference rules
// -----------------------------------

/** The goal was to have a minimal set of infernce rule
  * Those object ar only use for proof building
  * Please see the application of inference rules in LogicGraph for more details
  */

trait InferenceRule
case class ImplyIR(b: Boolean, implyPos: Int) extends InferenceRule
object FixIR extends InferenceRule
object FixLetSymIR extends InferenceRule
object SimplifyIR extends InferenceRule
object Axiom extends InferenceRule

object LogicGraph {
  def init: LogicGraph =
    LogicGraph().freshSymbolAssertEq(DefSymbol) |>
      (_.freshSymbolAssertEq(FalseSymbol)) |>
      (_.freshSymbolAssertEq(TrueSymbol)) |>
      (_.freshSymbolAssertEq(ImplySymbol)) |>
      (_.freshSymbolAssertEq(ForallSymbol)) |>
      (lg => lg.copy(truth = lg.truth + (lg.truePos -> true))) |>
      (lg => lg.copy(truth = lg.truth + (lg.falsePos -> false)))
}

case class LogicGraph(
    exprForest: ExprForest = new ExprForest,
    truth: Map[Int, Boolean] = Map(),
    imply: Map[Int, Set[Int]] = Map(),
    isImpliedBy: Map[Int, Set[Int]] = Map(),
    absurd: Option[(Int, Int)] = None,
    // use for proofs
    truthOrigin: Map[Int, Int] = Map(),
    inferences: Map[(Int, Int), InferenceRule] = Map()
) {

  def defPos = 0
  def falsePos = exprForest.getPos(FalseSymbol)
  def truePos = exprForest.getPos(TrueSymbol)
  def implyPos = exprForest.getPos(ImplySymbol)
  def forallPos = exprForest.getPos(ForallSymbol)

  def implyId = ImplySymbol.id

  def size = exprForest.size
  def idToPos(id: Int): Int = exprForest.idToPos(id)
  def getExprForest = exprForest
  def getExpr(pos: Int): Expr = exprForest.getExpr(pos)
  def getInferenceOf(a: Int, b: Int) = inferences get (a, b)
  def getTruthOriginOf(pos: Int) = truthOrigin get pos
  def getTruthOf(pos: Int) = truth get pos
  def getAbsurd = absurd
  def isAbsurd: Boolean = !absurd.isEmpty
  def getAllTruth(b: Boolean): Set[Int] = truth.filter(_._2 == b).keySet
  def getAllTruth: Set[Int] = truth.keySet
  def getHeadTail(p: Int): (Symbol, Seq[Int]) = exprForest.getHeadTail(p)
  def getHeadTailInt(p: Int): (Int, Seq[Int]) = exprForest.getHeadTailInt(p)

  def getImplies(p: Int): Set[Int] = imply.get(p) match {
    case None      => Set()
    case Some(set) => set
  }

  def isFixOf(fix: Int, pos: Int): Boolean = exprForest.getExpr(fix) match {
    case Fixer(next, _) => next == pos
    case _              => false
  }

  def isTruth(pos: Int, b: Boolean): Boolean = getTruthOf(pos) match {
    case Some(r) => r == b
    case None    => false
  }

  private def freshSymbolAssertEq(sym: Symbol): LogicGraph =
    getFreshSymbol match {
      case (lg, symbolPos) => {
        assert(lg.getExpr(symbolPos) == sym)
        lg
      }
    }

  def setAxiom(pos: Int, b: Boolean) =
    if (b) link(truePos, pos, Axiom) else link(pos, falsePos, Axiom)

  /** returns a new symbol position * */
  def getFreshSymbol: (LogicGraph, Int) =
    fix(defPos, exprForest.size) match {
      case (newLogicGraph, pos) => (newLogicGraph, pos - 1)
    }

  /** unfold forall into (inside, arguments) if any * */
  def unfoldForall(pos: Int): Option[(Int, Seq[Int])] =
    exprForest.getHeadTail(pos) match {
      case (ForallSymbol, inside +: args) => Some((inside, args))
      case _                              => None
    }

  def isFixable(pos: Int): Boolean =
    unfoldForall(
      pos
    ) match {
      case None                 => false
      case Some((inside, args)) => exprForest.countSymbols(inside) > args.length
    }

  // -------------------------------------------------------------
  // -------------------------------------------------------------
  // Applications of inference rules
  // -------------------------------------------------------------
  // -------------------------------------------------------------

  // note: when an infernce rule cannot by applied on pos,
  // it returns (this, pos)

  // -------------------------------------------------------------
  // Imply inference rule
  // -------------------------------------------------------------

  /** this method comtains 3 inference rules related to implications
    * - when A -> B is true then A => B
    * - when A -> B is false then true => A
    * - when A -> B is false then B => false
    */
  def implyInferenceRule(pos: Int): LogicGraph = {
    exprForest.getHeadTail(pos) match {
      case (ImplySymbol, Seq(a, b)) => {
        truth get pos match {
          case None => this
          case Some(true) =>
            link(a, b, ImplyIR(true, pos))
          case Some(false) =>
            link(truePos, a, ImplyIR(false, pos))
              .link(b, falsePos, ImplyIR(false, pos))
        }
      }
      case _ => this
    }
  }

  // -------------------------------------------------------------
  // simplify inference rule
  // -------------------------------------------------------------

  /** replace all symbols with corresponding args * */
  private def symplify(inside: Int, args: Seq[Int]): (LogicGraph, Int) =
    getExpr(
      inside
    ) match {
      case Symbol(id) => (this, args(id))
      case Fixer(next, arg) => {
        val (lgNext, posNext) = symplify(next, args)
        val (lgArg, posArg) = lgNext.symplify(arg, args)
        lgArg.fix(posNext, posArg)
      }
    }

  /** this method comtains 1 inference rule
    * when a forall contains n free Symbols and the tail/args of the forall
    * is of lenght at least n (ie: when all symbols in forall have been fixed)
    * then use simply (see simplify)
    */
  def symplifyInferenceRule(pos: Int): (LogicGraph, Int) = unfoldForall(
    pos
  ) match {
    case None => (this, pos)
    case Some((inside, args)) =>
      if (exprForest.countSymbols(inside) > args.length) (this, pos)
      else {
        val (logicGraph, newPos) = symplify(inside, args)
        (
          logicGraph
            .link(newPos, pos, SimplifyIR)
            .link(pos, newPos, SimplifyIR),
          newPos
        )
      }
  }

  /** use symplify in loop until there is nothing to simplify * */
  def symplifyInferenceRuleLoop(pos: Int): LogicGraph = {
    val (newLogicGraph, newPos) = symplifyInferenceRule(pos)
    if (newPos != pos) newLogicGraph.symplifyInferenceRule(newPos)._1
    else newLogicGraph
  }

  // -------------------------------------------------------------
  // fix inference rule
  // -------------------------------------------------------------

  // this section contains 2 inference rules

  /** when A is in the graph, then A => Fixer(A, B)
    */
  def fix(next: Int, arg: Int): (LogicGraph, Int) = {
    val (newExprForest, pos) = exprForest.fix(next, arg)
    copy(exprForest = newExprForest)
      .symplifyInferenceRuleLoop(pos)
      .link(next, pos, FixIR) match {
      case graph =>
        if (graph.getExprForest.isLetSymbol(next, arg))
          (graph.link(pos, next, FixLetSymIR), pos)
        else (graph, pos)
    }
  }

  def forall(body: Int): (LogicGraph, Int) = fix(forallPos, body)

  /** when A is in the graph, then it exists a symbol call
    * the LetSymbol of A (call it a) such that  A <=> Fixer(A, a)
    */
  def fixLetSymbol(pos: Int): (LogicGraph, Int) =
    exprForest.getLetSymbol(pos) match {
      case Some(posSymbol) => fix(pos, posSymbol)
      case None            => (this, pos)
    }

  // ---------------------------------------------------------------
  // method to propagate true/false value from one node to others
  // ---------------------------------------------------------------

  /** false propagate upward in implication while true propagate downward * */
  def getImplyGraphFor(b: Boolean) = if (b) imply else isImpliedBy

  /** propagate true/false in the graph * */
  private def propagate(pos: Int, prev: Int, b: Boolean): LogicGraph = {
    truth get pos match {
      case Some(v) if v == b => this

      // when asburd = Some((a,b)) it means a => b
      case Some(false) if b => copy(absurd = Some((prev, pos)))
      case Some(true) if !b => copy(absurd = Some((pos, prev)))

      case None => {

        val newLogicGraph = copy(
          truth = truth + (pos -> b),
          truthOrigin = truthOrigin + (pos -> prev)
        )
          .implyInferenceRule(pos)
          .symplifyInferenceRuleLoop(pos)

        // propagate truth value to neighbors
        newLogicGraph.getImplyGraphFor(b) get pos match {
          case None => newLogicGraph
          case Some(neighs: Set[Int]) => {
            neighs.foldLeft(newLogicGraph)((logicGraph, neigh) =>
              logicGraph.propagate(neigh, pos, b)
            )
          }
        }
      }
    }
  }

  /** insert a node as a neighbors of a in graph * */
  private def insertPair(a: Int, b: Int, graph: Map[Int, Set[Int]]) =
    graph get a match {
      case None    => graph + (a -> Set(b))
      case Some(s) => (graph - a) + (a -> (s + b))
    }

  /** create a imply link a => b in the graph and propagate changes* */
  private def link(a: Int, b: Int, inferenceRule: InferenceRule): LogicGraph = {
    require(a < exprForest.size && b < exprForest.size)

    if (a == b) this
    else {

      val newLogicGraph =
        copy(
          imply = insertPair(a, b, imply),
          isImpliedBy = insertPair(b, a, isImpliedBy),
          inferences = inferences + ((a, b) -> inferenceRule)
        )

      // A => B and A true: we propagate true to B
      val newLogicGraphPropagateOnB = truth get a match {
        case Some(true) => newLogicGraph.propagate(b, a, true)
        case _          => newLogicGraph
      }

      // A => B and B false: we propagate false to A
      val newLogicGraphPropagateOnA = truth get b match {
        case Some(false) => newLogicGraphPropagateOnB.propagate(a, b, false)
        case _           => newLogicGraphPropagateOnB
      }

      newLogicGraphPropagateOnA

    }

  }

}
