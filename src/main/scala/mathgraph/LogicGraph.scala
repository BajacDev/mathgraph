package mathgraph.mathgraph

import mathgraph.util.Pipe._

/** The logic layer manage the truth graph of expressions
  * Notation:
  * -> means the symbol imply (see below)
  * => means imply in the LogicGraph. when A => B, A is linked in the graph to B
  *
  * questions that have risen during the conception:
  *
  * why using '->' as a base Symbol and not 'not', 'or' and 'and'?
  * - I wanted to have the minimum of symbols. Moreover I wanted the base simbols
  *   to have a strong relationship with the graph we are manipulating. hence `->`
  *   directly have a instinclive meaning on the graph (see imply infrence rule).
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
object defSymbol extends Symbol(0)

object falseSymbol extends Symbol(1)
object trueSymbol extends Symbol(2)

/** implication symbol (eg: a -> b) * */
object implySymbol extends Symbol(3)

/** forall symbol * */
object forallSymbol extends Symbol(4)

// -----------------------------------
// inference rules
// -----------------------------------

/** The goal was to have a minimal set of infernce rule
  * Those object ar only use for proof building
  * Please see the application of inference rules in LogicGraph for more details
  */

trait InferenceRule
case class ImplyIR(b: Boolean, implyPos: Int) extends InferenceRule
object applyIR extends InferenceRule
object applyLetSymIR extends InferenceRule
object simplifyIR extends InferenceRule
object axiom extends InferenceRule

class LogicGraph(
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
  def falsePos = exprForest.getPos(falseSymbol)
  def truePos = exprForest.getPos(trueSymbol)
  def implyPos = exprForest.getPos(implySymbol)
  def forallPos = exprForest.getPos(forallSymbol)

  def size = exprForest.size
  def idToPos(id: Int): Int = exprForest.idToPos(id)
  def getExprForest = exprForest
  def getExpr(pos: Int): Expr = exprForest.getExpr(pos)
  def getInferenceOf(a: Int, b: Int) = inferences get (a, b)
  def getTruthOriginOf(pos: Int) = truthOrigin get pos
  def getTruthOf(pos: Int) = truth get pos
  def getAbsurd = absurd
  def isAbsurd: Boolean = !absurd.isEmpty

  private def freshSymbolAssertEq(sym: Symbol): LogicGraph =
    getFreshSymbol match {
      case (lg, symbolPos) => {
        assert(lg.getExpr(symbolPos) == sym)
        lg
      }
    }

  def init(): LogicGraph =
    freshSymbolAssertEq(defSymbol) |>
      (_.freshSymbolAssertEq(falseSymbol)) |>
      (_.freshSymbolAssertEq(trueSymbol)) |>
      (_.freshSymbolAssertEq(implySymbol)) |>
      (_.freshSymbolAssertEq(forallSymbol)) |>
      (lg => lg.addTruth(lg.truePos, true)) |>
      (lg => lg.addTruth(lg.falsePos, false))

  private def setExprForest(newExprForest: ExprForest) =
    new LogicGraph(
      newExprForest,
      truth,
      imply,
      isImpliedBy,
      absurd,
      truthOrigin,
      inferences
    )

  private def addTruth(pos: Int, b: Boolean) =
    new LogicGraph(
      exprForest,
      truth + (pos -> b),
      imply,
      isImpliedBy,
      absurd,
      truthOrigin,
      inferences
    )

  private def addTruth(pos: Int, prev: Int, b: Boolean) = {
    new LogicGraph(
      exprForest,
      truth + (pos -> b),
      imply,
      isImpliedBy,
      absurd,
      truthOrigin + (pos -> prev),
      inferences
    )
  }

  private def setAbsurd(pos: Int, prev: Int) =
    new LogicGraph(
      exprForest,
      truth,
      imply,
      isImpliedBy,
      Some((pos, prev)),
      truthOrigin,
      inferences
    )

  def setAxiom(pos: Int, b: Boolean) =
    if (b) link(truePos, pos, axiom) else link(pos, falsePos, axiom)

  /** returns a new symbol position * */
  def getFreshSymbol: (LogicGraph, Int) =
    apply(defPos, exprForest.size) match {
      case (newLogicGraph, pos) => (newLogicGraph, pos - 1)
    }

  /** unfold forall into (inside, arguments) if any * */
  def unfoldForall(pos: Int): Option[(Int, Seq[Int])] =
    exprForest.getHeadTail(pos) match {
      case (forallSymbol, inside +: args) => Some((inside, args))
      case _                              => None
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
      case (implySymbol, Seq(a, b)) => {
        truth get pos match {
          case None => this
          case Some(v) if v =>
            link(a, b, ImplyIR(true, pos))
          case Some(v) if !v =>
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
      case Apply(next, arg) => {
        val (lgNext, posNext) = symplify(next, args)
        val (lgArg, posArg) = lgNext.symplify(arg, args)
        lgArg.apply(posNext, posArg)
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
            .link(newPos, pos, simplifyIR)
            .link(pos, newPos, simplifyIR),
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
  // apply inference rule
  // -------------------------------------------------------------

  // this section contains 2 inference rules

  /** when A is in the graph, then A => Apply(A, B)
    */
  def apply(next: Int, arg: Int): (LogicGraph, Int) = {
    val (newExprForest, pos) = exprForest.apply(next, arg)
    setExprForest(newExprForest)
      .symplifyInferenceRuleLoop(pos)
      .link(next, pos, applyIR) match {
      case graph =>
        if (graph.getExprForest.isLetSymbol(next, arg))
          (graph.link(pos, next, applyLetSymIR), pos)
        else (graph, pos)
    }
  }

  /** when A is in the graph, then it exists a symbol call
    * the LetSymbol of A (call it a) such that  A <=> Apply(A, a)
    */
  def applyLetSymbol(pos: Int): (LogicGraph, Int) =
    exprForest.getLetSymbol(pos) match {
      case Some(posSymbol) => apply(posSymbol, pos)
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
      case Some(v) if v != b => setAbsurd(pos, prev)
      case None => {

        val newLogicGraph = addTruth(pos, prev, b)
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

    val newLogicGraph =
      new LogicGraph(
        exprForest,
        truth,
        insertPair(a, b, imply),
        insertPair(b, a, isImpliedBy),
        absurd,
        truthOrigin,
        inferences + ((a, b) -> inferenceRule)
      )

    // A => B and A true: we propagate true to B
    val newLogicGraphPropagateOnB = truth get a match {
      case Some(v) if v => newLogicGraph.propagate(b, a, v)
      case _            => newLogicGraph
    }

    // A => B and B false: we propagate false to A
    val newLogicGraphPropagateOnA = truth get b match {
      case Some(v) if !v => newLogicGraphPropagateOnB.propagate(a, b, v)
      case _             => newLogicGraphPropagateOnB
    }

    newLogicGraphPropagateOnA
  }

}
