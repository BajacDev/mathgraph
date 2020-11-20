package mathgraph.corelogic

/** The ExprForest manages Expressions without any logic assumption
  * The main point of this layer is to obtimize the storage of Expressions
  */

/** use Int instead of Expr tree:
  *  - for faster hash. (only hash int, not each expression in Fixer)
  *  - Symbols are only instanciated when needed
  *  - my c++ code use int, so I don't have to rethink the code entirely
  * avantage of using Expr instead of Int:
  *  - there is no need to go trough ExprForest to obtain next and arg Expr
  */

/** fixerToPosMap(a) gives the equivalent of fixerToPosMap indexOf a (but is faster)
  * so we use fixerToPosMap as a speedup mapping
  */
case class ExprForest(
    fixers: Seq[(Int, Int)] = Seq(),
    fixerToPosMap: Map[(Int, Int), Int] = Map()
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
    fixerToPosMap get newFixer match {
      case Some(pos) => (this, pos)
      case None =>
        (
          ExprForest(
            fixers :+ newFixer,
            fixerToPosMap + (newFixer -> nextFixerPos)
          ),
          nextFixerPos
        )
    }
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
