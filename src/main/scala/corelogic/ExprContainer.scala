package mathgraph.corelogic

import mathgraph.corelogic.ExprContainer._
import scala.math._

object ExprContainer {

  // todo: make it more explicit that they are position of symbols

  /** use to build other symbols * */
  val DefSymbol = 0 * 2

  val FalseSymbol = 1 * 2
  val TrueSymbol = 2 * 2

  /** implication symbol (eg: a -> b) * */
  val ImplySymbol = 3 * 2

  /** forall symbol * */
  val ForallSymbol = 4 * 2
}

trait ExprContainer {

  // Objects below implicitly use this function
  implicit protected def implicitThis: ExprContainer = this

  // from a expression position, return Some(Int, Int) if it is a Fixer
  // where the first one is the left subtree and the second one the right subtree
  def getFixer(pos: Int): Option[(Int, Int)]

  // from a expression position, return Some(Int) if it is a Symbol
  // the int is the symbol id
  def getSymbolId(pos: Int): Option[Int]

  // return an symbol position from the symbol id
  def idToSymbol(id: Int): Int = 2 * id

  /** retun the head and tail of an expression
    * head is the leftmost leaf in the tree
    * tail is the list of all right trees when accessiong this leftmost leaf
    */
  def getHeadTail(p: Int): (Int, Seq[Int]) = {

    def getHeadTailRec(pos: Int, args: Seq[Int]): (Int, Seq[Int]) = pos match {
      case Fixer(next, arg) => getHeadTailRec(next, arg +: args)
      case _                => (pos, args)
    }
    getHeadTailRec(p, Seq())
  }

  /** unfold forall into (inside, arguments) if any * */
  def unfoldForall(pos: Int): Option[(Int, Seq[Int])] =
    pos match {
      case HeadTail(ForallSymbol, inside +: args) => Some((inside, args))
      case _                                      => None
    }

  /** count the number of Fixer it would take to fix this expr
    * eg: returns 4 for 0(3, 1)
    */
  def countSymbols(pos: Int): Int = pos match {
    case Symbol(id)       => id + 1
    case Fixer(next, arg) => max(countSymbols(next), countSymbols(arg))
  }

}

object Fixer {
  def unapply(pos: Int)(implicit ec: ExprContainer): Option[(Int, Int)] =
    ec.getFixer(pos)
}

object Symbol {
  def unapply(pos: Int)(implicit ec: ExprContainer): Option[Int] =
    ec.getSymbolId(pos)
}

object HeadTail {
  def unapply(pos: Int)(implicit ec: ExprContainer): Option[(Int, Seq[Int])] =
    Some(ec.getHeadTail(pos))
}

object Forall {
  def unapply(pos: Int)(implicit ec: ExprContainer): Option[(Int, Seq[Int])] =
    ec.unfoldForall(pos)
}
