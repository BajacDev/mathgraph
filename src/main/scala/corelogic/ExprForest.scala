package mathgraph.corelogic

import scala.math._

/** The ExprForest manages Expressions without any logic assumption
  * The main point of this layer is to obtimize the storage of Expressions
  */

/** use Int instead of Expr:
  *  - for faster hash. (only hash int, not each expression in Fixer)
  *  - Symbols are only instanciated when needed
  *  - my c++ code use int, so I don't have to rethink the code entirely
  * avantage of using Expr instead of Int:
  *  - there is no need to go trough ExprForest to obtain next and arg Expr
  */

/** fixerToPos(a) gives the equivalent of fixerToPos indexOf a (but is faster)
  * so we use fixerToPos as a speedup mapping
  */
case class ExprForest(
    fixers: Seq[(Int, Int)] = Seq(),
    fixerToPos: Map[(Int, Int), Int] = Map()
) extends ExprContainer {

  def size = fixers.size * 2
  def nextFixerPos = size + 1

  def getFixer(pos: Int): Option[(Int, Int)] =
    if (pos % 2 == 1 && pos >= 0 && pos < size) Some(fixers(pos / 2))
    else None

  def getSymbolId(pos: Int): Option[Int] =
    if (pos % 2 == 0 && pos >= 0) Some(pos / 2)
    else None

  def fix(next: Int, arg: Int): (ExprForest, Int) = {
    require(next >= 0 && next < nextFixerPos && arg >= 0 && arg < nextFixerPos)

    val newFixer = (next, arg)
    // make sure we are not duplicating Applies
    fixerToPos get newFixer match {
      case Some(pos) => (this, pos)
      case None =>
        (
          ExprForest(
            fixers :+ newFixer,
            fixerToPos + (newFixer -> nextFixerPos)
          ),
          nextFixerPos
        )
    }
  }

  /** count the number of Fixer it would take to fix this expr
    * eg: returns 4 for 0(3, 1)
    */
  def countSymbols(pos: Int): Int = pos match {
    case Symbol(id)       => id + 1
    case Fixer(next, arg) => max(countSymbols(next), countSymbols(arg))
  }

  def getLetSymbol(pos: Int): Option[Int] = pos match {
    case Fixer(_, _) => Some(pos - 1)
    case _           => None
  }

  def isLetSymbol(next: Int, arg: Int): Boolean = getLetSymbol(
    next
  ) match {
    case Some(v) => v == arg
    case _       => false
  }

}
