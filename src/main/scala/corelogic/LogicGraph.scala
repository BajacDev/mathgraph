package mathgraph.corelogic

import mathgraph.util.Pipe._
import mathgraph.corelogic.ExprContainer._
import scala.collection.mutable.{Map, Set}

/** The logic layer manage the truth graph of expressions
  * Notation:
  * -> means the symbol imply (see below)
  * => means imply in the LogicGraph. when A => B, A is linked in the graph to B
  *
  * questions that have risen during the conception:
  *
  * why using '->' as a base Symbol and not 'not', 'or' and 'and'?
  * - I wanted to have the minimum number of symbols. Moreover I wanted the base symbols
  *   to have a strong relationship with the graph we are manipulating. hence `->`
  *   have a instinctive meaning on the graph (see imply infrence rule).
  *   It is not based on any performance requirements
  *
  * why ysing false propagation instead of just true propagation ?
  * - I wanted the minimum number of inference rules. using only true propagation
  *   would have increase the number of inference rule, specialy with
  *   negations of forall (exists). See future proof of test.txt
  */

// -----------------------------------------------------
// Logic Symbols
// -----------------------------------------------------

// -----------------------------------
// inference rules
// -----------------------------------

/** The goal was to have a minimal set of infernce rule
  * Those object ar only use for proof building
  * Please see the application of inference rules in LogicGraph for more details
  */

trait InferenceRule

// todo: some inference rules must and should be merged
case class ImplyIR(b: Boolean, implyPos: Int) extends InferenceRule
case class ImplyUpIR(a: Int, b: Int) extends InferenceRule
object SimplInsideIR extends InferenceRule
object FixIR extends InferenceRule
object FixLetSymIR extends InferenceRule
object SimplifyIR extends InferenceRule
object Axiom extends InferenceRule

object LogicGraph {
  def init: LogicGraph = {
    val lg = new LogicGraph()
    lg.freshSymbolAssertEq(DefSymbol)
    lg.freshSymbolAssertEq(FalseSymbol)
    lg.freshSymbolAssertEq(TrueSymbol)
    lg.freshSymbolAssertEq(ImplySymbol)
    lg.freshSymbolAssertEq(ForallSymbol)
    lg.truth += (TrueSymbol -> true)
    lg.truth += (FalseSymbol -> false)
    lg
  }
}

class LogicGraph extends ExprContainer {

  val exprForest: ExprForest = new ExprForest
  var truth: Map[Int, Boolean] = Map()
  var imply: Map[Int, Set[Int]] = Map()
  var isImpliedBy: Map[Int, Set[Int]] = Map()
  var absurd: Option[(Int, Int)] = None
  // use for proofs
  var truthOrigin: Map[Int, Int] = Map()
  var inferences: Map[(Int, Int), InferenceRule] = Map()

  def size = exprForest.size
  def getInferenceOf(a: Int, b: Int) = inferences get (a, b)
  def getTruthOriginOf(pos: Int) = truthOrigin get pos
  def getTruthOf(pos: Int) = truth get pos
  def getAbsurd = absurd
  def isAbsurd: Boolean = !absurd.isEmpty
  def getAllTruth(b: Boolean): Iterator[Int] =
    truth.filter(_._2 == b).keysIterator
  def getAllTruth: Iterator[Int] = truth.keysIterator

  def getFixer(pos: Int): Option[(Int, Int)] = exprForest.getFixer(pos)
  def fixerToPos(next: Int, arg: Int): Option[Int] =
    exprForest.fixerToPos(next, arg)
  def getSymbolId(pos: Int): Option[Int] = exprForest.getSymbolId(pos)

  def getImplies(p: Int): Set[Int] = imply.get(p) match {
    case None      => Set()
    case Some(set) => set
  }

  def isFixOf(fix: Int, pos: Int): Boolean = fix match {
    case Fixer(next, _) => next == pos
    case _              => false
  }

  def isTruth(pos: Int, b: Boolean): Boolean = getTruthOf(pos) match {
    case Some(r) => r == b
    case None    => false
  }

  private def freshSymbolAssertEq(pos: Int): Unit = {
    assert(getFreshSymbol == pos)
  }

  def setAxiom(pos: Int, b: Boolean) =
    if (b) link(TrueSymbol, pos, Axiom) else link(pos, FalseSymbol, Axiom)

  /** returns a new symbol position * */
  def getFreshSymbol: Int = {
    val pos = exprForest.size
    fix(DefSymbol, pos)
    pos
  }

  def isFixable(pos: Int): Boolean =
    pos match {
      case Forall(inside, args) => countSymbols(inside) > args.length
      case _                    => false
    }

  // -------------------------------------------------------------
  // -------------------------------------------------------------
  // Applications of inference rules
  // -------------------------------------------------------------
  // -------------------------------------------------------------

  // note: when an infernce rule cannot by applied on pos,
  // it returns (this, pos)

  // -------------------------------------------------------------
  // Imply inference rule
  // -------------------------------------------------------------

  /** this method comtains 3 inference rules related to implications
    * - when A -> B is true then A => B
    * - when A -> B is false then true => A
    * - when A -> B is false then B => false
    */
  def implyInferenceRule(pos: Int): Unit =
    pos match {
      case HeadTail(ImplySymbol, Seq(a, b)) => {
        truth get pos match {
          case None => ()
          case Some(true) => (getTruthOf(a), getTruthOf(b)) match {
            case (Some(v1), Some(v2)) if v1 == v2 => ()
            case _ => link(a, b, ImplyIR(true, pos))
          }
          case Some(false) => {
            if (!isTruth(a, true)) link(TrueSymbol, a, ImplyIR(false, pos))
            if (!isTruth(b, false)) link(b, FalseSymbol, ImplyIR(false, pos))
          }
        }
      }
      case _ => ()
    }

  def implyUpInferenceRule(pos: Int): Unit =
    (pos, getTruthOf(pos)) match {
      case (_, Some(_)) => ()
      case (HeadTail(ImplySymbol, Seq(a, b)), _) => {
        (truth get a, truth get b) match {
          case (Some(true), Some(true)) =>
            link(TrueSymbol, pos, ImplyUpIR(a, b))
          case (Some(false), Some(true)) =>
            link(TrueSymbol, pos, ImplyUpIR(a, b))
          case (Some(false), Some(false)) =>
            link(TrueSymbol, pos, ImplyUpIR(a, b))
          case (Some(true), Some(false)) =>
            link(pos, FalseSymbol, ImplyUpIR(a, b))
          case _ => ()
        }
      }
      case _ => ()
    }

  

  // -------------------------------------------------------------
  // simplify inference rule
  // -------------------------------------------------------------

  /** replace all symbols with corresponding args * */
  private def simplify(inside: Int, args: Seq[Int]): Int =
    inside match {
      case Symbol(id) => args(id)
      case Fixer(next, arg) => {
        fix(simplify(next, args), simplify(arg, args))
      }
    }

  /** this method comtains 1 inference rule
    * when a forall contains n free Symbols and the tail/args of the forall
    * is of length at least n (ie: when all symbols in forall have been fixed)
    * then use simply (see simplify)
    */
  def simplifyAll(pos: Int, toBefixed: Int): Option[Int] = pos match {
    case Forall(inside, args0) =>
      val args = args0 :+ toBefixed
      if (countSymbols(inside) > args.length) None
      else {
        val newPos = simplify(inside, args)
        Some(newPos)
      }
    case _ => None
  }

  def simplifyInside(orig: Int, toBeFixed: Int): Option[Int] = {
    (orig, toBeFixed) match {
      case (Forall(inside1, args), Forall(inside2, args2)) => 
        // todo: lots of duplicates

        val args1 = args :+ toBeFixed

        if (countSymbols(inside1) < args1.length) return None

        val shift = args2.length
        val newId = args.length
        val remain = countSymbols(inside2) - args2.length

        def willGenerateNewInside(pos: Int): Boolean = {
          pos match {
            case HeadTail(Symbol(id), seq) if id == newId && seq.length >= remain => true
            case Fixer(a, b) => willGenerateNewInside(a) || willGenerateNewInside(b)
            case Symbol(id) => false
          }
        }

        def simplifyInsideRec(pos: Int): Int = {
          pos match {
            case HeadTail(Symbol(id), seq) if id == newId && seq.length >= remain => 
              
              val mapping = (0 until args2.length).map(idToSymbol)
              simplify(inside2, mapping ++ seq.map(simplifyInsideRec))
            case Fixer(a, b) => 
              fix(simplifyInsideRec(a), simplifyInsideRec(b))
            case Symbol(id) => 
              idToSymbol(id + shift)
          }
        }

        if (!willGenerateNewInside(inside1)) return None

        val newInside = simplifyInsideRec(inside1)

        var pos = forall(newInside)
        val newArgs = (args2 ++ args1)
        for (arg <- newArgs) {
          pos = fix(pos, arg)
        }
        // todo
        Some(pos)
      case _ => None
    }
  }

  // -------------------------------------------------------------
  // fix inference rule
  // -------------------------------------------------------------

  // this section contains 2 inference rules

  /** when A is in the graph, then A => Fixer(A, B)
    */
  def fix(next: Int, arg: Int): Int = {
    val posOpt = simplifyAll(next, arg) match {
      case None => simplifyInside(next, arg)
      case s => s
    }

    val pos = posOpt match {
      case None => exprForest.fix(next, arg)
      case Some(s) => s
    }

    link(next, pos, FixIR)
    if (exprForest.isLetSymbol(next, arg)) {
      link(pos, next, FixLetSymIR)
      pos
    } else pos
  }

  def forall(body: Int): Int = fix(ForallSymbol, body)

  /** when A is in the graph, then it exists a symbol call
    * the LetSymbol of A (call it a) such that  A <=> Fixer(A, a)
    */
  def fixLetSymbol(pos: Int): Int =
    exprForest.getLetSymbol(pos) match {
      case Some(posSymbol) => fix(pos, posSymbol)
      case None            => pos
    }

  // ---------------------------------------------------------------
  // method to propagate true/false value from one node to others
  // ---------------------------------------------------------------

  /** false propagate upward in implication while true propagate downward * */
  def getImplyGraphFor(b: Boolean) = if (b) imply else isImpliedBy

  /** propagate true/false in the graph * */
  private def propagate(pos: Int, prev: Int, b: Boolean): Unit = {
    truth get pos match {
      case Some(v) if v == b => ()

      // when asburd = Some((a,b)) it means a => b
      case Some(false) if b => absurd = Some((prev, pos))
      case Some(true) if !b => absurd = Some((pos, prev))

      case None => {
        truth += (pos -> b)
        truthOrigin += (pos -> prev)
        implyInferenceRule(pos)

        // propagate truth value to neighbors
        val neighsOpt = getImplyGraphFor(b) get pos

        neighsOpt match {
          case None => ()
          case Some(neighs: Set[Int]) => {
            for (neigh <- neighs) {
              propagate(neigh, pos, b)
            }
          }
        }
      }
    }
  }

  def addBinding(map: Map[Int, Set[Int]], a: Int, b: Int): Unit = {
    map get a match {
      case None      => map(a) = Set(b)
      case Some(set) => set += b
    }
  }

  /** create a imply link a => b in the graph and propagate changes* */
  private def link(a: Int, b: Int, inferenceRule: InferenceRule): Unit = {
    require(a < exprForest.size && b < exprForest.size)

    if (a == b) return

    addBinding(imply, a, b)
    addBinding(isImpliedBy, b, a)
    inferences += ((a, b) -> inferenceRule)

    // A => B and A true: we propagate true to B
    truth get a match {
      case Some(true) => propagate(b, a, true)
      case _          => ()
    }

    // A => B and B false: we propagate false to A
    truth get b match {
      case Some(false) => propagate(a, b, false)
      case _           => ()
    }

    ()

  }

}
