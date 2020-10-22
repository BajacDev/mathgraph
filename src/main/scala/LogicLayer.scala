package mathgraph.mathgraph

/** The logic layer manage the truth of expressions
  */

object defSymbol extends Symbol(0)
object falseSymbol extends Symbol(1)
object trueSymbol extends Symbol(2)
object implySymbol extends Symbol(3)
object forallSymbol extends Symbol(4)



trait InferenceRule

object applyIR extends InferenceRule
object falseImplyIR extends InferenceRule
object trueImplyIR extends InferenceRule
object simplifyIR extends InferenceRule
object axiom extends InferenceRule

class LogicLayer(
    exprLayer: ExprLayer = new ExprLayer,
    truth: Map[Int, Boolean] = Map(),
    imply: Map[Int, Set[Int]] = Map(),
    isImpliedBy: Map[Int, Set[Int]] = Map(),
    absurd: Boolean = false,

    // use for proofs

    /** pos -> (previousPos, InferenceRule) **/
    inferences: Map[Int, (Int, InferenceRule)] = Map(),

    /** ((a, b) -> c) mean c is of the form a -> b **/
    reverseImply: Map[(Int, Int), Int] = Map(),

) {

  def truePos = exprLayer.getPos(trueSymbol)
  def falsePos = exprLayer.getPos(falseSymbol)
  def implyPos = exprLayer.getPos(implySymbol)
  def defPos = exprLayer.getPos(defSymbol)
  def forallPos = exprLayer.getPos(forallSymbol)

  def getAbsurd = absurd
  def getExprLayer = exprLayer

  def idToPos(id: Int): Int = exprLayer.idToPos(id)

  def init(): LogicLayer = {
    val newExprLayer = new ExprLayer()
      .setApply(defSymbol.id, defSymbol.id)
      ._1
      .setApply(defSymbol.id, falseSymbol.id)
      ._1
      .setApply(defSymbol.id, trueSymbol.id)
      ._1
      .setApply(defSymbol.id, forallSymbol.id)
      ._1
    val newTruth = Map(
      newExprLayer.getPos(falseSymbol) -> false,
      newExprLayer.getPos(trueSymbol) -> true
    )
    new LogicLayer(newExprLayer, newTruth)
  }

  def setExprLayer(newExprLayer: ExprLayer) =
    new LogicLayer(newExprLayer, truth, imply, isImpliedBy, absurd, inferences, reverseImply)

  def addReverseImply(a: Int, b: Int, imp: Int) =
    new LogicLayer(exprLayer, truth, imply, isImpliedBy, absurd, inferences, reverseImply + ((a, b) -> imp))

  def addTruth(pos: Int, b: Boolean) =
    if (b) link(truePos, pos, axiom) else link(pos, falsePos, axiom)

  def setAbsurd = new LogicLayer(exprLayer, truth, imply, isImpliedBy, true, inferences, reverseImply)

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
        (logicLayer.link(newPos, pos, simplifyIR).link(pos, newPos, simplifyIR), newPos)
      }
  }

  def applySymplifyInferenceRuleLoop(pos: Int): LogicLayer = {
    val (newLogicLayer, new_pos) = applySymplifyInferenceRule(pos)
    if (new_pos != pos) newLogicLayer.applySymplifyInferenceRule(new_pos)._1
    else newLogicLayer
  }

  // strict rule on imply: imply must be implySymbol and has an arity of 2
  def applyImplyInferenceRule(pos: Int): LogicLayer = {
    exprLayer.getHeadTail(pos) match {
      case (implySymbol, Seq(a, b)) => {
        truth get pos match {
          case None          => this
          case Some(v) if v  => link(a, b, trueImplyIR).addReverseImply(a, b, pos)
          case Some(v) if !v => link(truePos, a, falseImplyIR).link(b, falsePos, falseImplyIR)
            .addReverseImply(truePos, a, pos)
            .addReverseImply(b, falsePos, pos)
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
          (ll.link(pos, next, applyIR), pos)
        else (ll, pos)
    }
  }

  def getFreshSymbol: (LogicLayer, Int) =
    setApply(defPos, exprLayer.size) match {
      case (newLogicLayer, pos) => (newLogicLayer, pos - 1)
    }

  def getImplyGraphFor(b: Boolean) = if (b) imply else isImpliedBy

  /** propagate true/false in the graph * */
  def propagate(pos: Int, b: Boolean): LogicLayer = {
    truth get pos match {
      case Some(v) if v == b => this
      case Some(v) if v != b => setAbsurd
      case None => {
        val newThuth = truth + (pos -> b)
        val newLogicLayer =
          new LogicLayer(exprLayer, newThuth, imply, isImpliedBy, absurd, inferences, reverseImply)
            .applyImplyInferenceRule(pos)
            .applySymplifyInferenceRuleLoop(pos)

        // propagate truth value on the graph
        getImplyGraphFor(b) get pos match {
          case None => newLogicLayer
          case Some(neighs: Set[Int]) => {
            neighs.foldLeft(newLogicLayer)((logicUnit, neigh) =>
              logicUnit.propagate(neigh, b)
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
    val newImply = insertPair(a, b, imply)
    val newIsImpledBy = insertPair(b, a, isImpliedBy)

    val newLogicLayer =
      new LogicLayer(exprLayer, truth, newImply, newIsImpledBy, absurd, inferences + (b -> (a, inferenceRule)), reverseImply)

    // A => B and A true: we propagate true to B
    val newLogicLayerPropagateOnB = truth get a match {
      case Some(v) if v => newLogicLayer.propagate(b, v)
      case _            => newLogicLayer
    }

    // A => B and B false: we propagate false to A
    val newLogicLayerPropagateOnA = truth get b match {
      case Some(v) if !v => newLogicLayerPropagateOnB.propagate(a, v)
      case _             => newLogicLayerPropagateOnB
    }

    newLogicLayerPropagateOnA
  }

}
