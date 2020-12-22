package mathgraph.solver

import mathgraph.corelogic._
import mathgraph.corelogic.ExprContainer._
import mathgraph.util.Pipe._
import mathgraph.solver.Solver._
import mathgraph.printer._
import scala.collection.mutable.{Map => MutMap, Set => MutSet, ListBuffer}
import scala.math._

object Solver {
  val MAX_LOGICGRAPH_SIZE: Int = 10000
}

class Solver() {

  def saturation(iter: Option[Int])(implicit lg: LogicGraph): Unit = {

    if (iter.map(_ <= 0).getOrElse(false)) return

    if (lg.isAbsurd) return
    if (lg.size > MAX_LOGICGRAPH_SIZE) return

    val formerHash = lg.getGraphHash
    fixLetSym
    applyAllMgu
    disjunction

    if (formerHash == lg.getGraphHash) return

    saturation(iter.map(_ - 1))
  }

  def saturation(iter: Int)(implicit lg: LogicGraph): Unit = saturation(
    Some(iter)
  )

  def saturation()(implicit lg: LogicGraph): Unit = saturation(None)

  def isVariable(v: Int)(implicit lg: LogicGraph): Boolean = {
    val letFixer = (v + 1)
    letFixer match {
      case Forall(_, _) => lg.isTruth(letFixer, true)
      case _            => false
    }
  }

  def containsVariable(p: Int)(implicit lg: LogicGraph): Boolean = p match {
    case Fixer(ForallSymbol, _) => false
    case Fixer(a, b)            => containsVariable(a) || containsVariable(b)
    case _                      => isVariable(p)
  }

  /*
   * eg: return false for
   * mgu: y <- x + x
   * v, expr: x <- y + y
   */
  def cycleInMgu(v: Int, expr: Int, theta: Map[Int, Int])(implicit
      lg: LogicGraph
  ): Boolean = {
    if (v == expr) true
    else
      theta.get(expr) match {
        case Some(e) => cycleInMgu(v, e, theta)
        case None =>
          expr match {
            case Fixer(ForallSymbol, _) => false
            case Fixer(a, b) =>
              cycleInMgu(v, a, theta) || cycleInMgu(v, b, theta)
            case _ => false
          }
      }
  }

  def insertInMgu(p: Int, q: Int, theta: Map[Int, Int])(implicit
      lg: LogicGraph
  ): Option[Map[Int, Int]] = {
    // todo: make it faster
    theta.get(p) match {
      case Some(x) if x == q => Some(theta)
      case Some(x) if x != q => unify(x, q, theta)
      case None =>
        if (cycleInMgu(p, q, theta)) None
        else Some(theta + (p -> q))
    }
  }

  // fisrt order unify
  def unify(p: Int, q: Int, theta: Map[Int, Int])(implicit
      lg: LogicGraph
  ): Option[Map[Int, Int]] = {
    if (p == q) return Some(theta)
    else if (isVariable(p)) insertInMgu(p, q, theta)
    else if (isVariable(q)) insertInMgu(q, p, theta)
    else
      (p, q) match { // todo: foralls
        case (Fixer(ForallSymbol, _), Fixer(ForallSymbol, _)) => Some(theta)
        case (Fixer(l1, r1), Fixer(l2, r2)) =>
          unify(l1, l2, theta).flatMap(unify(r1, r2, _))
        case _ => None
      }
  }

  def findMguAbsurdity(
      orig: Int
  )(implicit lg: LogicGraph): Set[Map[Int, Int]] = {

    var visited: Map[Int, Boolean] = Map()

    def findAbsurd(
        pos: Int,
        b: Boolean,
        theta: Map[Int, Int]
    ): Option[Map[Int, Int]] = {

      def exploreImply: Option[Map[Int, Int]] = pos match {
        case HeadTail(ImplySymbol, Seq(left, right)) => {
          if (!b)
            findAbsurd(left, true, theta).orElse(
              findAbsurd(right, false, theta)
            )
          // todo: disjunction in disjunction?
          else if (visited.get(left) == Some(true))
            findAbsurd(right, true, theta)
          else if (visited.get(right) == Some(false))
            findAbsurd(left, false, theta)
          else None
        }
        case _ => None
      }

      def exploreNeighbors: Option[Map[Int, Int]] = {
        val neighsOpt = lg.getImplyGraphFor(b) get pos

        neighsOpt.flatMap(neighs => {
          neighs.toStream.map(findAbsurd(_, b, theta)).collectFirst {
            case Some(subst) => subst
          }
        })
      }

      def exploreMgu: Option[Map[Int, Int]] = {
        var result: Option[Map[Int, Int]] = None
        for (neigh <- 0 until lg.size) {
          // todo: !isVariable useful?
          if (neigh != pos && !isVariable(neigh)) {
            unify(pos, neigh, theta) match {
              case None => ()
              case Some(newTheta) =>
                result = result.orElse(findAbsurd(neigh, b, newTheta))
            }
          }
        }
        result
      }

      visited.get(pos) match {
        case Some(v) if v == b => {
          None
        }
        // todo: check mgu at node is possible with current mgu !!
        case Some(v) if v != b => {
          Some(theta)
        }
        case None => {
          visited = visited + (pos -> b)
          exploreImply.orElse(exploreNeighbors).orElse(exploreMgu)
        }
      }
    }

    var result: Set[Map[Int, Int]] = Set()

    // todo: better handling of cases with thruth(orig)
    visited = lg.truth.toMap - orig
    findAbsurd(orig, false, Map()) match {
      case None      => ()
      case Some(mgu) => result = result + mgu
    }

    visited = lg.truth.toMap - orig
    findAbsurd(orig, true, Map()) match {
      case None      => ()
      case Some(mgu) => result = result + mgu
    }

    result
  }

  def findAllMgu()(implicit lg: LogicGraph): Set[Map[Int, Int]] = {
    var result: Set[Map[Int, Int]] = Set()
    for (expr <- 0 until lg.size) {
      expr match {
        case HeadTail(ImplySymbol, Seq(a, b)) =>
          result = result ++ findMguAbsurdity(expr) ++ findMguAbsurdity(a) ++ findMguAbsurdity(b)
        case _ => ()
      }
    }
    result
  }

  def createFromMgu(p: Int, theta: Map[Int, Int])(implicit
      lg: LogicGraph
  ): Int = theta.get(p) match {
    case None => p
    case Some(value) =>
      value match {
        case e @ Fixer(ForallSymbol, _) => e
        case Fixer(a, b) =>
          lg.fix(createFromMgu(a, theta), createFromMgu(b, theta))
        case e @ _ => createFromMgu(e, theta)
      }
  }

  def applyMgu(theta: Map[Int, Int])(implicit lg: LogicGraph): Unit = {
    for ((v, rep) <- theta.toSeq.sortBy(_._1)) {
      val letFixer = (v + 1)
      lg.fix(letFixer, createFromMgu(v, theta))
    }
  }

  def applyAllMgu()(implicit lg: LogicGraph): Unit = {
    findAllMgu.foreach(applyMgu)
  }

  /** fix all expressions with their let symbol
    * for now, I fix every false expression because I did not find
    * proofs where  you have to fix true expression. That can change
    */
  def fixLetSym()(implicit lg: LogicGraph): Unit = {
    // todo: less brut force
    for (expr <- 0 until lg.size) {
      if (lg.getTruthOf(expr).isEmpty) ()
      else
        expr match {
          case Forall(inside, args) => lg.fixLetSymbol(expr)
          case _                    => ()
        }
    }
  }

  def disjunction()(implicit lg: LogicGraph): Unit = {
    for (expr <- 0 until lg.size) {
      expr match {
        // sensitive part: can't use disjunction on anything (bc of correctness)
        // move it into logic graph
        case HeadTail(ImplySymbol, Seq(a, b)) =>
          lg.disjunction(expr)
          lg.disjunction(a)
          lg.disjunction(b)
        case _ => ()
      }
    }
  }

}
