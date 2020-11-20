package mathgraph.printer

import mathgraph.corelogic._
import io.AnsiColor._
import mathgraph.corelogic.ExprContainer._
import mathgraph.frontend.Trees._

case class Printer(
    exprToString: Map[Int, String]
) {

  def get(pos: Int) = exprToString get pos

  def remove(pos: Int): Printer = Printer(exprToString - pos)

  /** print expression in a simple way * */
  def toSimpleString(implicit logicGraph: LogicGraph, pos: Int): String =
    pos match {
      case HeadTail(Symbol(id), Seq()) => id.toString
      case HeadTail(Symbol(id), seq) =>
        id.toString + "(" + seq
          .map(toSimpleString(logicGraph, _))
          .mkString(", ") + ")"
    }

  // ---------------------------------------------------------------------------
  // print an humain readable expression using exprToString map
  // ---------------------------------------------------------------------------

  /** generate names for Symbol. eg: 0: 'a0', 1: 'b0', ..., 26: 'a1'
    */
  def generateName(id: Int): String = {
    val char = (id % 26 + 'a'.toInt).toChar
    s"$char${(id / 26)}"
  }

  def generateNameForall(id: Int): String = {
    require(id >= 0 && id < 26)
    (id % 26 + 'a'.toInt).toChar.toString
  }

  def infix(s: String) = !s.matches("^[a-zA-Z0-9_]*$")

  // apply Parentheses
  def applyPar(p: (String, Boolean)): String = p match {
    case (s, true) => s"($s)"
    case (s, false) => s
  }

  def combineHeadTail(head: String, tail: List[(String, Boolean)]): (String, Boolean) = tail match {
    case x :: y :: Nil if infix(head) => (applyPar(x) + " " + head + " " + applyPar(y), true)
    case Nil           => (head, false)
    case _             => (head + "(" + tail.map(_._1).mkString(", ") + ")", false)
  }

  /**
  * replace the first vars in body for multiforall by fixes
  */
  def replace(body: Expr, vars: Seq[String], fixes: Seq[Expr]): Expr = {

    def replaceRec(expr: Expr, map: Map[String, Expr]): Expr = expr match {
      case Apply(name, Seq()) if map.contains(name) => map(name)
      case Apply(name, seq) => Apply(name, seq.map(replaceRec(_, map)))
      case MultiForall(v, b) => MultiForall(v, replaceRec(b, map))
    }

    val (oldVars, newVars) = vars.splitAt(fixes.length)
    val map = oldVars.zip(fixes).map{case (a, b) => a -> b}.toMap
    MultiForall(newVars, replaceRec(body, map))
  }

  /** convert a pos in logicgraph to a string
  * first convert to frontend tree, then to String
  **/
  def toString(implicit logicGraph: LogicGraph, orig: Int): String = {

    def toExpr(orig: Int, map: Map[Int, Expr]): Expr = {

      def toExprRec(pos: Int, seq: Seq[Expr]): Expr = 
        map.get(pos) match {
          case Some(Apply(name, args)) => Apply(name, args ++ seq)
          case Some(MultiForall(vars, body)) => replace(body, vars, seq)
          case _ => pos match {
            case Fixer(ForallSymbol, arg) => forallToExpr(arg, seq)
            case Fixer(next, arg) => toExprRec(next, toExpr(arg, map) +: seq)
            case Symbol(id) => Apply(generateName(id), seq)
          }
        }
      toExprRec(orig, Seq())
    }

    def forallToExpr(inside: Int, args: Seq[Expr]): Expr = {
      val numVars = logicGraph.countSymbols(inside)
      val numFixedVar = args.length
      val freeVars = (numFixedVar until numVars).map(generateNameForall)
      val varsInside = args ++ freeVars.map(Apply(_, Seq()))
      val map = varsInside.zipWithIndex.map{
        case (expr, id) => logicGraph.idToSymbol(id) -> expr
      }.toMap
      MultiForall(freeVars, toExpr(inside, map))
    }

    def simplifyExpr(expr: Expr): Expr = expr match {
      case Apply("->", Seq(lhs, Apply("false", Seq()))) => Apply("not", Seq(lhs))
      case Apply(id, seq) => Apply(id, seq.map(simplifyExpr))
      case MultiForall(vars, body) => MultiForall(vars, simplifyExpr(body))
    }

    def toString(expr: Expr): (String, Boolean) = expr match {
      case Apply(head, tail) => combineHeadTail(head, tail.map(toString).toList)
      case MultiForall(freeVars, body) => (s"forall ${freeVars.mkString(", ")}. ${toString(body)._1}", false)
    }

    val map = exprToString.view.mapValues(Apply(_, Seq())).toMap
    toString(simplifyExpr(toExpr(orig, map)))._1
  }

  // ------------------------------------------
  // function to print a human readable proofs
  // ------------------------------------------

  // TODO: create a new class. Make it less String dependent

  /** list of expressions st way(0) -> way(1) -> ... -> way(n) * */
  def proofFromList(
      lg: LogicGraph,
      way: List[Int],
      alg: String
  ): List[String] = {

    def lineToString(pos: Int) = alg + toString(lg, pos)

    way match {
      case Nil     => Nil
      case List(a) => List(lineToString(a))
      case a :: b :: xs => {

        val nextProof = proofFromList(lg, b :: xs, alg)

        val nextLayerProof = lg.getInferenceOf(a, b) match {
          case Some(ImplyIR(_, implyPos)) =>
            proofFromPos(lg, implyPos, alg + "\u2193 ")
          case _ => List()
        }

        lineToString(a) :: (nextLayerProof ++ nextProof)
      }
    }
  }

  def proofFromPos(lg: LogicGraph, pos: Int, alg: String = ""): List[String] = {
    proofFromList(lg, wayToTruth(lg, pos), alg)
  }

  /** build list of expressions st way(0) -> way(1) -> ... -> way(n) * */
  def wayToTruth(logicGraph: LogicGraph, orig: Int): List[Int] = {
    def wayToTruthRec(pos: Int): List[Int] =
      logicGraph.getTruthOriginOf(pos) match {
        case None          => List(pos)
        case Some(nextPos) => pos :: wayToTruthRec(nextPos)
      }

    logicGraph.getTruthOf(orig) match {
      case None        => Nil
      case Some(true)  => wayToTruthRec(orig).reverse
      case Some(false) => wayToTruthRec(orig)
    }
  }

  /** build the way from true to false and create the proof * */
  def proofAbsurd(lg: LogicGraph): List[String] = lg.getAbsurd match {
    case None => Nil
    case Some((a, b)) =>
      proofFromList(lg, wayToTruth(lg, a) ++ wayToTruth(lg, b), "")
  }

}
