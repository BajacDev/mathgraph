package mathgraph.mathgraph

/**
* The logic layer manage the truth of expressions
**/

object defSymbol extends Symbol(0)
object falseSymbol extends Symbol(1)
object trueSymbol extends Symbol(2)
object implySymbol extends Symbol(3)
object forallSymbol extends Symbol(4)

class LogicLayer(exprLayer: ExprLayer = new ExprLayer, truth: Map[Int, Boolean] = Map(), 
    imply: Map[Int, Set[Int]] = Map(), isImpliedBy: Map[Int, Set[Int]] = Map(), absurd: Boolean = false) {

    def truePos = exprLayer.getPos(trueSymbol)
    def falsePos = exprLayer.getPos(falseSymbol)
    def implyPos = exprLayer.getPos(implySymbol)

    def getAbsurd = absurd
    
    def init(): LogicLayer = {
        val newExprLayer = new ExprLayer()
            .setApply(defSymbol.id, defSymbol.id)._2
            .setApply(defSymbol.id, falseSymbol.id)._2
            .setApply(defSymbol.id, trueSymbol.id)._2
            .setApply(defSymbol.id, forallSymbol.id)._2
        val newTruth = Map(newExprLayer.getPos(falseSymbol) -> false, newExprLayer.getPos(trueSymbol) -> true)
        new LogicLayer(newExprLayer, newTruth, Map(), Map(), false)
    }

    def setExprLayer(newExprLayer: ExprLayer) = new LogicLayer(newExprLayer, truth, imply, isImpliedBy, absurd)

    def addTruth(pos: Int, b: Boolean) = if (b) link(truePos, pos) else link(pos, falsePos) 

    def setAbsurd = new LogicLayer(exprLayer, truth, imply, isImpliedBy, true)

    def setApply(next: Int, arg: Int): (Int, LogicLayer) = {
        val (pos, newExprLayer) = exprLayer.setApply(next, arg)
        (pos, setExprLayer(newExprLayer))
    }

    def is(b: Boolean, pos: Int) = truth get(pos) match {
        case None => false
        case Some(v) => v
    }

    // strict rule on imply: imply must be implySymbol and has an arity of 2
    def applyImplyInferenceRule(pos: Int): LogicLayer = {
        exprLayer.getHeadTail(pos) match {
            case (implySymbol, Seq(a, b)) => {
                truth get pos match {
                    case None => this
                    case Some(v) if v => link(a, b)
                    case Some(v) if !v => link(truePos, a).link(b, falsePos)
                }
            }
            case _ => this
        }
    }

    def getImplyGraphFor(b: Boolean) = if(b) imply else isImpliedBy

    /** propagate true/false in the graph **/
    def propagate(pos: Int, b: Boolean): LogicLayer = {
        truth get pos match {
            case Some(v) if v == b => this
            case Some(v) if v != b => setAbsurd
            case None => {
                val newThuth = truth + (pos -> b)
                val newLogicLayer = new LogicLayer(exprLayer, newThuth, imply, isImpliedBy, absurd)
                    .applyImplyInferenceRule(pos)

                // propagate truth value on the graph
                getImplyGraphFor(b) get pos match {
                    case None => newLogicLayer
                    case Some(neighs: Set[Int]) => {
                        neighs.foldLeft(newLogicLayer)((logicUnit, neigh) => logicUnit.propagate(neigh, b))
                    }
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