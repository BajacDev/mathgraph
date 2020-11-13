package mathgraph.corelogic

import scala.math._

/** The ExprForest manages Expressions without any logic assumption
  * The main point of this layer is to obtimize the storage of Expressions
  */

abstract class Expr

case class Symbol(id: Int) extends Expr

/** use Int instead of Expr:
  *  - for faster hash. (only hash int, not each expression in Apply)
  *  - Symbols are only instanciated when needed
  *  - my c++ code use int, so I don't have to rethink the code entirely
  * avantage of using Expr instead of Int:
  *  - there is no need to go trough ExprForest to obtain next and arg Expr
  */
case class Apply(next: Int, arg: Int) extends Expr

/** applyToPos(a) gives the equivalent of applyToPos indexOf a (but is faster)
  * so we use applyToPos as a speedup mapping
  */
case class ExprForest(
    applies: Seq[Apply] = Seq(),
    applyToPos: Map[Apply, Int] = Map()
) {

  def size = applies.size * 2
  def nextApplyPos = size + 1

  /** returns expression according to pos
    * note: there is as much symbol as Applies. this is used have enough LetSymbols
    */
  def getExpr(pos: Int): Expr = {
    if (pos % 2 == 0) Symbol(pos / 2)
    else {
      require(pos >= 0 && pos < size)
      applies(pos / 2)
    }
  }

  def apply(next: Int, arg: Int): (ExprForest, Int) = {
    require(next >= 0 && next < nextApplyPos && arg >= 0 && arg < nextApplyPos)

    val newApply = Apply(next, arg)
    // make sure we are not duplicating Applies
    applyToPos get newApply match {
      case Some(pos) => (this, pos)
      case None =>
        (
          ExprForest(
            applies :+ newApply,
            applyToPos + (newApply -> nextApplyPos)
          ),
          nextApplyPos
        )
    }
  }

  def getPos(expr: Expr): Int = expr match {
    case Symbol(id) => {
      val pos = id * 2
      pos
    }
    case a: Apply => {
      require(applyToPos contains a)
      applyToPos(a)
    }
  }

  def idToPos(id: Int): Int = getPos(Symbol(id))

  /** count the number of Apply it would take to fix this expr
    * eg: returns 4 for 0(3, 1)
    */
  def countSymbols(pos: Int): Int = getExpr(pos) match {
    case Symbol(id)       => id + 1
    case Apply(next, arg) => max(countSymbols(next), countSymbols(arg))
  }

  /** retun the head and tail of an expression
    * head is the leftmost leaf in the tree
    * tail is the list of all right trees when accessiong this leftmost leaf
    */
  def getHeadTail(p: Int): (Symbol, Seq[Int]) = {
    def getHeadTailRec(pos: Int, args: Seq[Int]): (Symbol, Seq[Int]) = getExpr(
      pos
    ) match {
      case s: Symbol        => (s, args)
      case Apply(next, arg) => getHeadTailRec(next, arg +: args)
    }
    getHeadTailRec(p, Seq())
  }

  def getHeadTailInt(p: Int): (Int, Seq[Int]) = {
    def getHeadTailRec(pos: Int, args: Seq[Int]): (Int, Seq[Int]) = getExpr(
      pos
    ) match {
      case Apply(next, arg) => getHeadTailRec(next, arg +: args)
      case _                => (pos, args)
    }
    getHeadTailRec(p, Seq())
  }

  def getHead(pos: Int): Symbol = getHeadTail(pos)._1

  def getTail(pos: Int): Seq[Int] = getHeadTail(pos)._2

  def getLetSymbol(pos: Int): Option[Int] = getExpr(pos) match {
    case expr: Expr => Some(pos - 1)
    case _          => None
  }

  def isLetSymbol(next: Int, arg: Int): Boolean = getLetSymbol(
    next
  ) match {
    case Some(v) => v == arg
    case _       => false
  }

}
