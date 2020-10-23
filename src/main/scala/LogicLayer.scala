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

class LogicLayer(
    exprLayer: ExprLayer = new ExprLayer,
    truth: Map[Int, Boolean] = Map(),
    imply: Map[Int, Set[Int]] = Map(),
    isImpliedBy: Map[Int, Set[Int]] = Map(),
    absurd: Option[(Int, Int)] = None,
    // use for proofs
    truthOrigin: Map[Int, Int] = Map(),
    inferences: Map[(Int, Int), InferenceRule] = Map()
) {

  def defPos = 0
  def falsePos = exprLayer.getPos(falseSymbol)
  def truePos = exprLayer.getPos(trueSymbol)
  def implyPos = exprLayer.getPos(implySymbol)
  def forallPos = exprLayer.getPos(forallSymbol)

  def isAbsurd: Boolean = !absurd.isEmpty
  def getExprLayer = exprLayer
  def getInferenceOf(a: Int, b: Int) = inferences get (a, b)
  def getTruthOriginOf(pos: Int) = truthOrigin get pos
  def getTruthOf(pos: Int) = truth get pos

  def getAbsurd = absurd

  def idToPos(id: Int): Int = exprLayer.idToPos(id)

  private def freshSymbolAssertEq(sym: Symbol): LogicLayer =
    getFreshSymbol match {
      case (ll, symbolPos) => {
        assert(ll.getExprLayer.getExpr(symbolPos) == sym)
        ll
      }
    }

  def init(): LogicLayer =
    freshSymbolAssertEq(defSymbol) |>
      (_.freshSymbolAssertEq(falseSymbol)) |>
      (_.freshSymbolAssertEq(trueSymbol)) |>
      (_.freshSymbolAssertEq(implySymbol)) |>
      (_.freshSymbolAssertEq(forallSymbol)) |>
      (ll => ll.addTruth(ll.truePos, true)) |>
      (ll => ll.addTruth(ll.falsePos, false))

  def setExprLayer(newExprLayer: ExprLayer) =
    new LogicLayer(
      newExprLayer,
      truth,
      imply,
      isImpliedBy,
      absurd,
      truthOrigin,
      inferences
    )

  private def addTruth(pos: Int, b: Boolean) =
    new LogicLayer(
      exprLayer,
      truth + (pos -> b),
      imply,
      isImpliedBy,
      absurd,
      truthOrigin,
      inferences
    )

  private def addTruth(pos: Int, prev: Int, b: Boolean) = {
    new LogicLayer(
      exprLayer,
      truth + (pos -> b),
      imply,
      isImpliedBy,
      absurd,
      truthOrigin + (pos -> prev),
      inferences
    )
  }

  def setAbsurd(pos: Int, prev: Int) = new LogicLayer(
    exprLayer,
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
    exprLayer.getHeadTail(pos) match {
      case (forallSymbol, inside +: args) => Some((inside, args))
      case _                              => None
    }

  /** return true if it is a forall and it is simplifiable * */
  def isSimplifiable(pos: Int): Boolean = unfoldForall(pos) match {
    case None                 => false
    case Some((inside, args)) => exprLayer.countSymbols(inside) <= args.length
  }

  def symplify(inside: Int, args: Seq[Int]): (LogicLayer, Int) =
    exprLayer.getExpr(
      inside
    ) match {
      case Symbol(id) => (this, args(id))
      case Apply(next, arg) => {
        val (logicLayerNext, posNext) = symplify(next, args)
        val (logicLayerArg, posArg) = logicLayerNext.symplify(arg, args)
        logicLayerArg.setApply(posNext, posArg)
      }
    }

  def applySymplifyInferenceRule(pos: Int): (LogicLayer, Int) = unfoldForall(
    pos
  ) match {
    case None => (this, pos)
    case Some((inside, args)) =>
      if (exprLayer.countSymbols(inside) > args.length) (this, pos)
      else {
        val (logicLayer, newPos) = symplify(inside, args)
        (
          logicLayer
            .link(newPos, pos, simplifyIR)
            .link(pos, newPos, simplifyIR),
          newPos
        )
      }
  }

  def applySymplifyInferenceRuleLoop(pos: Int): LogicLayer = {
    val (newLogicLayer, newPos) = applySymplifyInferenceRule(pos)
    if (newPos != pos) newLogicLayer.applySymplifyInferenceRule(newPos)._1
    else newLogicLayer
  }

  // strict rule on imply: imply must be implySymbol and has an arity of 2
  def applyImplyInferenceRule(pos: Int): LogicLayer = {
    exprLayer.getHeadTail(pos) match {
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

  def applyAssociatedSymbol(pos: Int): (LogicLayer, Int) =
    exprLayer.getAssociatedSymbol(pos) match {
      case Some(posSymbol) => setApply(posSymbol, pos)
      case None            => (this, pos)
    }

  def setApply(next: Int, arg: Int): (LogicLayer, Int) = {
    val (newExprLayer, pos) = exprLayer.setApply(next, arg)
    setExprLayer(newExprLayer)
      .applySymplifyInferenceRuleLoop(pos)
      .link(next, pos, applyIR) match {
      case ll =>
        if (ll.getExprLayer.isAssociatedSymbol(next, arg))
          (ll.link(pos, next, applyLetSymIR), pos)
        else (ll, pos)
    }
  }

  def getFreshSymbol: (LogicLayer, Int) =
    setApply(defPos, exprLayer.size) match {
      case (newLogicLayer, pos) => (newLogicLayer, pos - 1)
    }

  def getImplyGraphFor(b: Boolean) = if (b) imply else isImpliedBy

  /** propagate true/false in the graph * */
  def propagate(pos: Int, prev: Int, b: Boolean): LogicLayer = {
    truth get pos match {
      case Some(v) if v == b => this
      case Some(v) if v != b => setAbsurd(pos, prev)
      case None => {

        val newLogicLayer = addTruth(pos, prev, b)
          .applyImplyInferenceRule(pos)

        // propagate truth value on the graph
        newLogicLayer.getImplyGraphFor(b) get pos match {
          case None => newLogicLayer
          case Some(neighs: Set[Int]) => {
            neighs.foldLeft(newLogicLayer)((logicUnit, neigh) =>
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
  def link(a: Int, b: Int, inferenceRule: InferenceRule): LogicLayer = {
    require(a < exprLayer.size && b < exprLayer.size)

    val newLogicLayer =
      new LogicLayer(
        exprLayer,
        truth,
        insertPair(a, b, imply),
        insertPair(b, a, isImpliedBy),
        absurd,
        truthOrigin,
        inferences + ((a, b) -> inferenceRule)
      )

    // A => B and A true: we propagate true to B
    val newLogicLayerPropagateOnB = truth get a match {
      case Some(v) if v => newLogicLayer.propagate(b, a, v)
      case _            => newLogicLayer
    }

    // A => B and B false: we propagate false to A
    val newLogicLayerPropagateOnA = truth get b match {
      case Some(v) if !v => newLogicLayerPropagateOnB.propagate(a, b, v)
      case _             => newLogicLayerPropagateOnB
    }

    newLogicLayerPropagateOnA
  }

}
