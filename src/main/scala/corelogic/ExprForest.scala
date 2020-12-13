package mathgraph.corelogic
import scala.collection.mutable.{ArrayBuffer, Map}

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
class ExprForest extends ExprContainer {

  var fixers: ArrayBuffer[(Int, Int)] = ArrayBuffer()
  var fixerToPosMap: Map[(Int, Int), Int] = Map()

  val a = 5
  val w = 5;
  var counter = 0

  var weights: Map[Int, Int] = Map()
  var weightQueue: List[Int] = List()
  var ageQueue: List[Int] = List()

  def getWeight(pos: Int): Int = {
    // Symbols have a weight of 1
    if (pos < 0 || pos >= size) Int.MaxValue
    else if (pos % 2 == 0) 1
    else weights.getOrElse(pos, Int.MaxValue)
  }

  def enqueueFixer(pos: Int): Unit = {
    ageQueue = pos :: ageQueue

    val weight = getWeight(pos)
    val (smallerWeight, higherWeight) =
      weightQueue.span(fixerPos => getWeight(fixerPos) < weight)
    weightQueue = smallerWeight ++ List(pos) ++ higherWeight
  }

  def dequeueFixer(list: List[Int]): (Option[Int], List[Int]) = list match {
    case head :: tail => (Some(head), tail :+ head)
    case Nil          => (None, Nil)
  }

  def selectFixer(): Option[Int] = counter match {
    case i if i < a => {
      counter += 1
      val (result, updatedQueue) = dequeueFixer(ageQueue)
      ageQueue = updatedQueue
      result
    }
    case i if i < a + w => {
      counter += 1
      val (result, updatedQueue) = dequeueFixer(weightQueue)
      weightQueue = updatedQueue
      result
    }
    case _ => {
      counter = 0
      selectFixer()
    }
  }

  def size = fixers.size * 2
  def nextFixerPos = size + 1

  def getFixer(pos: Int): Option[(Int, Int)] =
    if (pos % 2 == 1 && pos >= 0 && pos < size) Some(fixers(pos / 2))
    else None

  def fixerToPos(next: Int, arg: Int): Option[Int] = {
    fixerToPosMap get (next, arg)
  }

  def getSymbolId(pos: Int): Option[Int] =
    if (pos % 2 == 0 && pos >= 0) Some(pos / 2)
    else None

  def fix(next: Int, arg: Int): Int = {
    require(next >= 0 && next < nextFixerPos && arg >= 0 && arg < nextFixerPos)

    val newFixer = (next, arg)
    // make sure we are not duplicating Applies
    fixerToPosMap get newFixer match {
      case Some(pos) => pos
      case None => {
        val fixerPos = nextFixerPos
        fixers += newFixer
        fixerToPosMap += (newFixer -> fixerPos)
        weights += (fixerPos -> (getWeight(next) + getWeight(
          arg
        ))) // todo: + 1?
        enqueueFixer(fixerPos)
        fixerPos
      }
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
