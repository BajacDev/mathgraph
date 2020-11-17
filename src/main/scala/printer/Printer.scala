package mathgraph.printer

import mathgraph.corelogic._
import io.AnsiColor._

case class Printer(
    exprToString: Map[Int, String]
) {

  /** print expression in a simple way * */
  def toSimpleString(logicGraph: LogicGraph, pos: Int): String = {
    // todo find a better way to access this function (too much dots)
    logicGraph.getHeadTail(pos) match {
      case (Symbol(id), Seq()) => id.toString
      case (Symbol(id), seq) =>
        id.toString + "(" + seq
          .map(toSimpleString(logicGraph, _))
          .mkString(", ") + ")"
    }
  }

  // ---------------------------------------------------------------------------
  // print an humain readable expression using exprToString map
  // ---------------------------------------------------------------------------

  // todo: make forall printing more humain friendly (eg: forall x, y, z. P(x, y, z))

  /** generate names for Symbol. eg: 0: 'a', 1: 'b', ..., 26: 'a1'
    * gives color to symbol according to
    * - what symbol is left to be fixed (BLUE)
    * - what is the next symbol to be fixed (MAGENTA)
    * id: the symbol id
    * numArg: the number of args outside of the forall (if any)
    */
  def generateName(id: Int, numArgs: Option[Int]): String = {
    val char = (id % 26 + 'a'.toInt).toChar
    val result = if (id < 26) char.toString else s"$char${(id / 26)}"
    numArgs match {
      case Some(n) if id == n => s"${MAGENTA}${result}${RESET}"
      case Some(n) if id > n  => s"${BLUE}${result}${RESET}"
      case _                  => result
    }
  }

  def combineHeadTail(head: String, tail: List[String]): String = tail match {
    case Nil           => head
    case x :: y :: Nil => "(" + x + " " + head + " " + y + ")"
    case _             => head + "(" + tail.mkString(", ") + ")"
  }

  def toString(logicGraph: LogicGraph, orig: Int): String = {

    def listToMap(stringList: List[String]): Map[Int, String] = {
      stringList.zipWithIndex.map { case (str, idx) =>
        (logicGraph.idToPos(idx) -> str)
      }.toMap
    }

    /** print the expression at position origin* */
    def toString(orig: Int): String = {
      def toStringRec(
          pos: Int,
          args: List[Int],
          exprNames: Map[Int, String],
          forallPos: Option[Int]
      ): String = {

        def numArgs: Option[Int] = forallPos match {
          case None => Some(exprNames.size)
          case _    => None
        }

        def argsToString(argList: List[Int]): List[String] = {
          argList.map(toStringRec(_, Nil, exprNames, forallPos))
        }

        exprNames get pos match {
          case Some(name) if Some(pos) != forallPos =>
            combineHeadTail(name, argsToString(args))
          case None =>
            logicGraph.getExpr(pos) match {
              case Symbol(id) =>
                forallPos match {
                  case Some(p) if p == pos =>
                    args match {
                      case Nil =>
                        combineHeadTail(generateName(id, numArgs), Nil)
                      case s :: xs =>
                        "{" + toStringRec(
                          s,
                          Nil,
                          listToMap(argsToString(xs)),
                          None
                        ) + "}"
                    }
                  case _ =>
                    combineHeadTail(
                      generateName(id, numArgs),
                      argsToString(args)
                    )
                }
              case Apply(next, arg) =>
                toStringRec(next, arg :: args, exprNames, forallPos)
            }
        }
      }
      toStringRec(orig, Nil, exprToString, Some(logicGraph.forallPos))
    }

    toString(orig)
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
    way match {
      case Nil     => Nil
      case List(a) => List(alg + toString(lg, a))
      case a :: b :: xs =>
        lg.getInferenceOf(a, b) match {
          case None => Nil // todo: assert(false)
          case Some(ImplyIR(_, implyPos)) =>
            (alg + toString(lg, a)) :: proofFromPos(
              lg,
              implyPos,
              alg + "\u2193 "
            ) ++ proofFromList(lg, b :: xs, alg)
          case _ => (alg + toString(lg, a)) :: proofFromList(lg, b :: xs, alg)
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
