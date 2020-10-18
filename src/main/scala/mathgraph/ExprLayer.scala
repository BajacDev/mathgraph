package mathgraph.mathgraph

/**
* The ExprLayer manages Expressions without any logic assumption
* The main point of this layer is to obtimize the storage of Expressions
**/

abstract class Expr

case class Symbol(id: Int) extends Expr

// use Int instead of Expr:
//  - for faster hash. (only hash int, not each expression in Apply)
//    https://stackoverflow.com/questions/4526706/what-code-is-generated-for-an-equals-hashcode-method-of-a-case-class
//  - Symbols are only instanciated when needed
//  - my c++ code use int, so I don't have to rethink the code entirely
// avantage of using Expr instead of Int:
//  - there is no need to go trough ExprLayer to obtain next and arg Expr
case class Apply(next: Int, arg: Int) extends Expr

// applyToPos(a) gives the equivalent of applyToPos indexOf a (but is faster)

class ExprLayer(applies: Seq[Apply] = Seq(), applyToPos: Map[Apply, Int] = Map()) {

    def size = applies.size * 2
    def nextExprPos = size + 1

    def getExpr(pos: Int): Expr = {
        require( pos < size )
        if (pos % 2 == 0) Symbol(pos / 2)
        else applies(pos / 2)
    }

    // there is no setSymbol: adding an expr automatically add a symbol at pos = new expr pos - 1
    def setApply(next: Int, arg: Int): (Int, ExprLayer) = {
        require( next < nextExprPos && arg < nextExprPos )
        val apply = Apply(next, arg)
        applyToPos get apply match {
            case Some(pos) => (pos, this)
            case None => (nextExprPos, new ExprLayer(applies :+ apply, applyToPos + (apply -> nextExprPos)))
        }
    }

    def getPos(expr: Expr): Int = expr match {
        case Symbol(id) => {
            val pos = id * 2
            require(pos < size)
            pos
        }
        case apply: Apply => {
            require(applyToPos contains apply)
            applyToPos(apply)
        }
    }

    def getHead(pos: Int): Int = getExpr(pos) match {
        case Symbol(_) => pos
        case Apply(next, _) => getHead(next)
    }

    def getHeadTail(p: Int): (Symbol, Seq[Int]) = {
        def getHeadTailRec(pos: Int, args: Seq[Int]): (Symbol, Seq[Int]) = getExpr(pos) match {
            case s: Symbol => (s, args)
            case Apply(next, arg) => getHeadTailRec(next, arg +: args)
        }
        getHeadTailRec(p, Seq())
    }

    def getTail(pos: Int): Seq[Int] = getHeadTail(pos)._2

}