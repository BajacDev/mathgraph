package mathgraph.mathgraph

import mathgraph.util.Pipe._

/** The logic layer manage the truth of expressions
  */

object defSymbol extends Symbol(0)
object falseSymbol extends Symbol(1)
object trueSymbol extends Symbol(2)
object implySymbol extends Symbol(3)
object forallSymbol extends Symbol(4)

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

  def isAbsurd: Boolean = !absurd.isEmpty
  def getExprForest = exprForest
  def getInferenceOf(a: Int, b: Int) = inferences get (a, b)
  def getTruthOriginOf(pos: Int) = truthOrigin get pos
  def getTruthOf(pos: Int) = truth get pos

  def getAbsurd = absurd

  def idToPos(id: Int): Int = exprForest.idToPos(id)

  private def freshSymbolAssertEq(sym: Symbol): LogicGraph =
    getFreshSymbol match {
      case (lg, symbolPos) => {
        assert(lg.getExprForest.getExpr(symbolPos) == sym)
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

  def setExprForest(newExprForest: ExprForest) =
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

  def setAbsurd(pos: Int, prev: Int) = new LogicGraph(
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

  def is(b: Boolean, pos: Int) = truth get (pos) match {
    case None    => false
    case Some(v) => v == b
  }

  def unfoldForall(pos: Int): Option[(Int, Seq[Int])] =
    exprForest.getHeadTail(pos) match {
      case (forallSymbol, inside +: args) => Some((inside, args))
      case _                              => None
    }

  /** return true if it is a forall and it is simplifiable * */
  def isSimplifiable(pos: Int): Boolean = unfoldForall(pos) match {
    case None                 => false
    case Some((inside, args)) => exprForest.countSymbols(inside) <= args.length
  }

  def symplify(inside: Int, args: Seq[Int]): (LogicGraph, Int) =
    exprForest.getExpr(
      inside
    ) match {
      case Symbol(id) => (this, args(id))
      case Apply(next, arg) => {
        val (logicGraphNext, posNext) = symplify(next, args)
        val (logicGraphArg, posArg) = logicGraphNext.symplify(arg, args)
        logicGraphArg.setApply(posNext, posArg)
      }
    }

  def applySymplifyInferenceRule(pos: Int): (LogicGraph, Int) = unfoldForall(
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

  def applySymplifyInferenceRuleLoop(pos: Int): LogicGraph = {
    val (newLogicGraph, newPos) = applySymplifyInferenceRule(pos)
    if (newPos != pos) newLogicGraph.applySymplifyInferenceRule(newPos)._1
    else newLogicGraph
  }

  // strict rule on imply: imply must be implySymbol and has an arity of 2
  def applyImplyInferenceRule(pos: Int): LogicGraph = {
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

  def applyAssociatedSymbol(pos: Int): (LogicGraph, Int) =
    exprForest.getAssociatedSymbol(pos) match {
      case Some(posSymbol) => setApply(posSymbol, pos)
      case None            => (this, pos)
    }

  def setApply(next: Int, arg: Int): (LogicGraph, Int) = {
    val (newExprForest, pos) = exprForest.setApply(next, arg)
    setExprForest(newExprForest)
      .applySymplifyInferenceRuleLoop(pos)
      .link(next, pos, applyIR) match {
      case graph =>
        if (graph.getExprForest.isAssociatedSymbol(next, arg))
          (graph.link(pos, next, applyLetSymIR), pos)
        else (graph, pos)
    }
  }

  def getFreshSymbol: (LogicGraph, Int) =
    setApply(defPos, exprForest.size) match {
      case (newLogicGraph, pos) => (newLogicGraph, pos - 1)
    }

  def getImplyGraphFor(b: Boolean) = if (b) imply else isImpliedBy

  /** propagate true/false in the graph * */
  def propagate(pos: Int, prev: Int, b: Boolean): LogicGraph = {
    truth get pos match {
      case Some(v) if v == b => this
      case Some(v) if v != b => setAbsurd(pos, prev)
      case None => {

        val newLogicGraph = addTruth(pos, prev, b)
          .applyImplyInferenceRule(pos)

        // propagate truth value on the graph
        newLogicGraph.getImplyGraphFor(b) get pos match {
          case None => newLogicGraph
          case Some(neighs: Set[Int]) => {
            neighs.foldLeft(newLogicGraph)((logicUnit, neigh) =>
              logicUnit.propagate(neigh, pos, b)
            )
          }
        }
      }
    }
  }

  // utils method
  def insertPair(a: Int, b: Int, imp: Map[Int, Set[Int]]) =
    imp get a match {
      case None    => imp + (a -> Set(b))
      case Some(s) => (imp - a) + (a -> (s + b))
    }

  /** create a imply link a => b in the graph and propagate changes* */
  def link(a: Int, b: Int, inferenceRule: InferenceRule): LogicGraph = {
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