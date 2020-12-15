package mathgraph.corelogic

import mathgraph.util.Pipe._
import mathgraph.corelogic.ExprContainer._
import scala.collection.immutable.{Map, Set}
import scala.collection.mutable.{Map => MutMap, Set => MutSet}

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
object Disjonction extends InferenceRule

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
  // todo use mulatble
  var truth: MutMap[Int, Boolean] = MutMap()
  var imply: MutMap[Int, MutSet[Int]] = MutMap()
  var isImpliedBy: MutMap[Int, MutSet[Int]] = MutMap()
  var absurd: Option[(Int, Int)] = None
  // use for proofs
  var truthOrigin: MutMap[Int, Int] = MutMap()
  var inferences: MutMap[(Int, Int), InferenceRule] = MutMap()

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

  def getImplies(p: Int): MutSet[Int] = imply.get(p) match {
    case None      => MutSet()
    case Some(set) => set
  }

  def getIsImpliedBy(p: Int): MutSet[Int] = isImpliedBy.get(p) match {
    case None      => MutSet()
    case Some(set) => set
  }

  def isFix(a: Int, b: Int): Boolean = inferences.get((a, b)) == Some(FixIR)

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
          case Some(true) =>
            (getTruthOf(a), getTruthOf(b)) match {
              case (Some(v1), Some(v2)) if v1 == v2 => ()
              case _                                => link(a, b, ImplyIR(true, pos))
            }
          case Some(false) => {
            if (!isTruth(a, true)) link(TrueSymbol, a, ImplyIR(false, pos))
            if (!isTruth(b, false)) link(b, FalseSymbol, ImplyIR(false, pos))
          }
        }
      }
      case _ => ()
    }

  def disjonction(orig: Int): Unit = {

    var visited: Map[Int, Boolean] = truth.toMap

    def findAbsurd(pos: Int, b: Boolean): Boolean = {

      def exploreImply: Boolean = pos match {
        case HeadTail(ImplySymbol, Seq(left, right)) => {
          if (!b) findAbsurd(left, true) || findAbsurd(right, false)
          // todo: disjonction in disjonction?
          else if (visited.get(left) == Some(true)) findAbsurd(right, true)
          else if (visited.get(right) == Some(false)) findAbsurd(left, false)
          else false
        }
        case _ => false
      }

      def exploreNeighbors: Boolean = {
        val neighsOpt = getImplyGraphFor(b) get pos

        neighsOpt match {
          case None => false
          case Some(neighs: MutSet[Int]) => {
            var v = false
            for (neigh <- neighs) {
              v = v || findAbsurd(neigh, b)
            }
            v
          }
        }
      }

      visited.get(pos) match {
        case Some(v) if v == b => false
        case Some(v) if v != b => true
        case None => {
          visited = visited + (pos -> b)
          exploreImply || exploreNeighbors
        }
      }
    }

    if (findAbsurd(orig, false)) {
      link(TrueSymbol, orig, Disjonction)
    }
    if (findAbsurd(orig, true)) {
      link(orig, FalseSymbol, Disjonction)
    }
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
  def simplifyAll(pos: Int): Option[Int] = pos match {
    case Forall(inside, args) =>
      if (countSymbols(inside) > args.length) None
      else {

        val result = simplify(inside, args)
        link(result, pos, SimplifyIR)
        link(pos, result, SimplifyIR)
        Some(result)
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
            case HeadTail(Symbol(id), seq)
                if id == newId && seq.length >= remain =>
              true
            case Fixer(a, b) =>
              willGenerateNewInside(a) || willGenerateNewInside(b)
            case Symbol(id) => false
          }
        }

        def simplifyInsideRec(pos: Int): Int = {
          pos match {
            case HeadTail(Symbol(id), seq)
                if id == newId && seq.length >= remain =>
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

  def getForallFalseFrom(inside: Int, args: Seq[Int]): Int = {
    val imp = idToSymbol(args.indexOf(ImplySymbol))
    val bot = idToSymbol(args.indexOf(FalseSymbol))
    var pos = imp
    pos = fix(pos, inside)
    pos = fix(pos, bot)
    pos = getForallFrom(pos, args)
    pos = fix(ImplySymbol, pos)
    pos = fix(pos, FalseSymbol)
    pos
  }

  def splitForall(orig: Int): Unit = {

    def extract(inside: Int, args: Seq[Int]) = {
      val l = args.length
      inside match {
        case HeadTail(
              Symbol(imp1),
              Seq(HeadTail(Symbol(imp2), Seq(a, b)), Symbol(f))
            ) =>
          (args.lift(imp1), args.lift(imp2), args.lift(f)) match {
            case (Some(ImplySymbol), Some(ImplySymbol), Some(FalseSymbol)) =>
              Some((a, b))
            case _ => None
          }
        case _ => None
      }
    }

    orig match {
      case Forall(inside, args) if isTruth(orig, true) =>
        extract(inside, args) match {
          case Some((a, b)) => {
            val aForall = getForallFrom(a, args)
            val bForall = getForallFalseFrom(b, args)

            var pos = ImplySymbol
            pos = fix(pos, aForall)
            pos = fix(pos, bForall)
            pos = fix(ImplySymbol, pos)
            pos = fix(pos, FalseSymbol)

            link(pos, orig, SimplifyIR) // todo change name
            link(orig, pos, SimplifyIR)
          }
          case None => ()
        }
      case _ => ()
    }
  }

  /// todooooo

  class ArgsMapping {
    // expr to Symbols
    var map: Map[Int, Int] = Map()
    var argPos = 0;

    def init(args: Seq[Int]): Unit = {
      for (elem <- args) {
        create(elem)
      }
      argPos = args.length
    }

    def create(pos: Int): Unit = map.get(pos) match {
      case None => 
        map = map + (pos -> idToSymbol(map.size))
        ()
      case Some(_) => ()
    }

    def get(pos: Int): Int = map(pos)

    def getAllArgs: Seq[Int] = {
      val getInvMapping: Map[Int, Int] = map.map(_.swap)
      for (i <- 0 until map.size) 
        yield getInvMapping(idToSymbol(i))
    }

    def symbolIdMapping(id: Int): Int = {
      if (id > argPos) idToSymbol(id - argPos - 1 + map.size)
      else idToSymbol(id)
    }
  }

  def getPlacesInside(inside: Int, len: Int): Set[Int] = {
    def getPlaceInsideRec(pos: Int): Set[Int] = pos match {
      case HeadTail(Symbol(id), seq) if id == len => {
        seq.toSet.flatMap(getPlaceInsideRec) + pos
      }
      case HeadTail(Symbol(id), seq) => {
        seq.toSet.flatMap(getPlaceInsideRec)
      }
    }
    getPlaceInsideRec(inside)
  }

  def splitImplyInside(inside: Int, args: Seq[Int]): Option[(Int, Int)] = inside match {
    case HeadTail(imp, Seq(a, b)) if args.lift(imp) == Some(ImplySymbol) => Some((a, b))
    case _ => None
  }

  def getHeadTailFrom(head: Int, tail: Seq[Int]): Int = {
    var pos = head
    for (arg <- tail) {
      pos = fix(pos, arg)
    }
    pos
  }


  def getForallFrom(inside: Int, args: Seq[Int]): Int = {
    getHeadTailFrom(forall(inside), args)
  }

  def getExprWhenApplied(pos: Int, numFix: Int): Set[Int] = pos match {
    case Forall(inside, args) => getExprWhenAppliedForall(inside, args, numFix)
    case HeadTail(head, tail) => tail.toSet.flatMap(getExprWhenApplied(_, numFix)) + head
  }

  def getExprWhenAppliedForall(inside: Int, args: Seq[Int], numFix: Int): Set[Int] = {
    val numSymbols = countSymbols(inside)
    if (numSymbols <= args.length + numFix) args.toSet.flatMap(getExprWhenApplied(_, numFix))
    else splitImplyInside(inside, args) match {
      case Some((a, b)) if countSymbols(a) <= args.length + numFix => 
        (getExprWhenAppliedForall(a, args, numFix) ++ getExprWhenAppliedForall(b, args, numFix)) + ImplySymbol
      case _ => Set(getForallFrom(inside, args))
    }
  }

  /////

  def applyInside(newArg: Int, seq: Seq[Int], mapping: Map[Int, Int]): Int = newArg match {
    case Forall(inside, args) => applyInsideForall(inside, args, seq, mapping)
    case HeadTail(head, tail) => {
      getHeadTailFrom(mapping(head), tail.map(applyInside(_, Seq(), mapping)) ++ seq)
    }

  }

  def applyInsideForall(inside: Int, args: Seq[Int], seq: Seq[Int], mapping: Map[Int, Int]): Int = {
    val numSymbols = countSymbols(inside)
    if (numSymbols <= args.length + seq.length) {
      val newArgs = args.map(applyInside(_, Seq(), mapping)) ++ seq
      simplify(inside, newArgs)
    }
    else splitImplyInside(inside, args) match {
      case Some((a, b)) if countSymbols(a) <= args.length + seq.length =>
        val newArgs = args.map(applyInside(_, Seq(), mapping)) ++ seq
        var pos = mapping(ImplySymbol)
        pos = fix(pos, simplify(a, newArgs))
        pos = fix(pos, applyInsideForall(b, args, seq, mapping))
        pos
      case _ => {
        val outsideForall = getForallFrom(inside, args)
        val remain = countSymbols(inside) - args.length
        getHeadTailFrom(mapping(outsideForall), seq.take(remain))
      }
    }
  }

  def toSimpleString(pos: Int)(implicit logicGraph: LogicGraph): String =
    pos match {
    case HeadTail(Symbol(id), Seq()) => id.toString
    case HeadTail(Symbol(id), seq) =>
      id.toString + "(" + seq
        .map(toSimpleString)
        .mkString(", ") + ")"
  }

  ////

  // todo: we should be able to fix multiple items ate the same time in order
  // to decrase the number of propositions
  def buildMappingAndFix(inside: Int, args: Seq[Int], newArg: Int): Int = {

    val argsMapping = new ArgsMapping()
    argsMapping.init(args)

    val placeSet = getPlacesInside(inside, args.length)

    val allArgs = placeSet.flatMap(_ match {
      case HeadTail(_, tail) => getExprWhenApplied(newArg, tail.length)
    })

    //println(toSimpleString(inside)(this))
    //println(args)
    //println(argsMapping.map)
    //println(toSimpleString(newArg)(this))
    //println(allArgs, newArg)


    allArgs.foreach(argsMapping.create)

    def computeNewInside(pos: Int, argNum: Int): Int = pos match {
      case HeadTail(Symbol(id), tail) if id == argNum => {
        val newTail = tail.map(computeNewInside(_, argNum))
        applyInside(newArg, newTail, argsMapping.map)
      }
      case HeadTail(Symbol(id), tail) => 
        getHeadTailFrom(argsMapping.symbolIdMapping(id), tail.map(computeNewInside(_, argNum)))
    }

    val newInside = computeNewInside(inside, args.length)
    getForallFrom(newInside, argsMapping.getAllArgs)
  }

  /// todooooo

  def fixAndSimpl(next: Int, arg: Int): Int = next match {
    case Forall(inside, args) => 
      if (countSymbols(inside) <= args.length) next
      else {
        val pos = fix(next, arg)//buildMappingAndFix(inside, args, arg)
        link(next, pos, FixIR)
        if (exprForest.isLetSymbol(next, arg)) {
          link(pos, next, FixLetSymIR)
        }
        pos
      }
    case _ => fix(next, arg)
  }

  // -------------------------------------------------------------
  // fix inference rule
  // -------------------------------------------------------------

  // this section contains 2 inference rules

  /** when A is in the graph, then A => Fixer(A, B)
    */
  def fix(next: Int, arg: Int): Int = {

    /*val canBeFixed: Boolean = next match {
      case Forall(inside, args) => countSymbols(inside) > args.length
      case _                    => true
    }

    val pos = simplifyInside(next, arg) match {
      case None    => if (canBeFixed) exprForest.fix(next, arg) else next
      case Some(s) => s
    }*/

    val pos = exprForest.fix(next, arg)


    link(next, pos, FixIR)
    if (exprForest.isLetSymbol(next, arg)) {
      link(pos, next, FixLetSymIR)
    }
    pos
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
        //splitForall(pos)
        simplifyAll(pos)

        // propagate truth value to neighbors
        val neighsOpt = getImplyGraphFor(b) get pos

        neighsOpt match {
          case None => ()
          case Some(neighs: MutSet[Int]) => {
            for (neigh <- neighs) {
              propagate(neigh, pos, b)
            }
          }
        }
      }
    }
  }

  def addBinding(map: MutMap[Int, MutSet[Int]], a: Int, b: Int): Unit = {
    map get a match {
      case None      => map(a) = MutSet(b)
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
