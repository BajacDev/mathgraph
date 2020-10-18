package mathgraph.mathgraph

/**
* The logic layer manage the truth of expression.
* 
**/

case object defSymbol extends Symbol(0)
case object falseSymbol extends Symbol(1)
case object trueSymbol extends Symbol(2)
case object implySymbol extends Symbol(3)
case object forAllSymbol extends Symbol(4)

class LogicLayer(exprLayer: ExprLayer, truth: Map[Int, Boolean], 
    imply: Map[Int, Set[Int]], isImpliedBy: Map[Int, Set[Int]], absurd: Boolean) {

    def init(): LogicLayer = {
        val newExprLayer = exprLayer
            .setApply(defSymbol id, defSymbol id)._2
            .setApply(defSymbol id, falseSymbol id)._2
            .setApply(defSymbol id, trueSymbol id)._2
            .setApply(defSymbol id, forAllSymbol id)._2
        val newTruth = truth 
            + (newExprLayer.getPos(falseSymbol) -> false)
            + (newExprLayer.getPos(trueSymbol) -> true)
        new LogicLayer(newExprLayer, newTruth, imply, isImpliedBy, false)
    }

    def setExprLayer(newexprLayer: ExprLayer) = new LogicLayer(newExprLayer, truth, imply, isImpliedBy, absurd)

    def setApply(next: Int, arg: Int): (Int, LogicLayer) = {
        // intentionnal design: allowing forallSymbol to be an arg inside a forall makes thing difficult 
        // when we need to count the number of symbols to fix inside this forall
        // eg: {0(3, 1)}(forall)
        // how many symbols inside? basic algo: 4, advance algo that take the forall into account: 2
        require( exprLayer.getExpr arg != implySymbol )
        val pos, newExprLayer = exprLayer.setApply(next, arg)
        (pos, setExprLayer(newExprLayer))
    }

    def setAbsurd = new LogicLayer(exprLayer, truth, imply, isImpliedBy, True)

    def is(b: Boolean, pos: Int) = truth get pos match {
        case None => false
        case Some(v) => v
    }

    // strict rule on imply: imply is implySymbol and has arity 2
    def applyImplyInferenceRule(pos: Int): LogicLayer = exprLayer.getHeadTail pos match {
        case (implySymbol, Seq(a, b)) => {
            truth get pos match {
                case None => this
                case Some(b) if b => link(a, b)
                case Some(b) if !b => link(trueSymbol, a).link(b, falseSymbol)
            }
        }
        case _ => this
    }

    def getImplyGraphFor(b: Boolean) = if(b) imply else isImpliedBy


    def propagate(pos: Int, b: Boolean): LogicLayer = 
        truth get pos match {
            case Some(v) if v == b => this
            case Some(v) if v != b => setAbsurd
            case None => {
                val newThuth = truth + (pos -> b)
                val newLogicLayer = new LogicLayer(exprLayer, newThuth, imply, isImpliedBy, absurd)
                    .applyImplyInferenceRule(pos)

                // propagate truth value on the graph
                getImplyGraphFor(b) get pos match => {
                    case None => newLogicLayer
                    case Some(neighs: Set[Int]) => {
                        neigh.foldLeft(newLogicLayer)((logicUnit, neigh) => logicUnit.propagate(neigh, b))
                    }
                }
            }
        }
    
    // utils method
    def insertPair(a: Int, b: Int, imp: Map[Int, Set[Int]]) = 
        imp get a match {
            case None => imp + (a -> Set(b))
            case Some(s) => (imp - a) + (a -> (s + b))
        }
        
    

    /** create a imply link a => b in the graph and propagate changes**/
    def link(a: Int, b: Int): LogicLayer = {
        require(a < exprLayer.size && b < exprLayer.size)
        val newImply = insertPair(a, b, imply)
        val newIsImpledBy = insertPair(b, a, isImpliedBy)



        val newLogicLayer = new LogicLayer(exprLayer, truth, newImply, newIsImpledBy, absurd)

        // A => B and A true: we propagate true to B
        val newLogicLayerPropagateOnB = truth get a match {
            case Some(v) if v => newLogicLayer.propagate(b, v)
            case _ => newLogicLayer
        }

        // A => B and B false: we propagate false to A
        val newLogicLayerPropagateOnA = truth get b match {
            case Some(v) if !v => newLogicLayerPropagateOnB.propagate(a, v)
            case _ => newLogicLayerPropagateOnB
        }

        newLogicLayerPropagateOnA   
    }

}