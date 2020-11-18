package mathgraph.corelogic

import mathgraph.corelogic.ExprContainer._

object ExprContainer {

  /** use to build other symbols * */
  val DefSymbol = (0) * 2

  val FalseSymbol = (1) * 2
  val TrueSymbol = (2) * 2

  /** implication symbol (eg: a -> b) * */
  val ImplySymbol = (3) * 2

  /** forall symbol * */
  val ForallSymbol = (4) * 2
}

trait ExprContainer {

  implicit protected def implicitThis: ExprContainer = this

  def getFixer(pos: Int): Option[(Int, Int)]

  def getSymbolId(pos: Int): Option[Int]
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
