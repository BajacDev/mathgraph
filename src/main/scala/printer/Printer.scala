package mathgraph.printer

import mathgraph.mathgraph._
import io.AnsiColor._

class Printer(
    logicLayer: LogicLayer,
    exprToString: Map[Int, String] = Map()
) {

  def init(): Printer = {
    val e2s = Map(
      logicLayer.defPos -> "def",
      logicLayer.falsePos -> "false",
      logicLayer.truePos -> "true",
      logicLayer.implyPos -> "->"
    )
    new Printer(logicLayer, e2s)
  }

  /** print expression in a simple way * */
  def toSimpleString(pos: Int): String = {
    // todo find a better way to access this function (too much dots)
    logicLayer.getExprForest.getHeadTail(pos) match {
      case (Symbol(id), Seq()) => id.toString
      case (Symbol(id), seq) =>
        id.toString + "(" + seq.map(toSimpleString).mkString(", ") + ")"
    }
  }

  // ---------------------------------------------------------------------------
  // print an humain readable expression using exprToString map
  // ---------------------------------------------------------------------------

  /** generate names for Symbol. eg: 0: 'a', 1: 'b', ..., 26: 'a1'
    * gives color to symbol according to
    * - what symbol is left to be fixed (MAGENTA)
    * - what is the next symbol to be fixed (BLUE)
    * id: the symbol id
    * numArg: the number of args outside of the forall (if any)
    */
  def generateName(id: Int, numArgs: Option[Int]): String = {
    val char = (id % 26 + 'a'.toInt).toChar
    val result = if (id < 26) char.toString else char + (id / 26).toString
    numArgs match {
      case Some(n) if id == n => s"${BLUE}${result}${RESET}"
      case Some(n) if id > n  => s"${MAGENTA}${result}${RESET}"
      case _                  => result
    }
  }

  def combineHeadTail(head: String, tail: List[String]): String = tail match {
    case Nil           => head
    case x :: y :: Nil => "(" + x + " " + head + " " + y + ")"
    case _             => head + "(" + tail.mkString(", ") + ")"
  }

  def listToMap(stringList: List[String]): Map[Int, String] = {
    def listToMapRec(
        l: List[String],
        id: Int,
        result: Map[Int, String]
    ): Map[Int, String] = l match {
      case Nil => result
      case x :: xs =>
        listToMapRec(xs, id + 1, result + (logicLayer.idToPos(id) -> x))
    }
    listToMapRec(stringList, 0, Map())
  }

  /** print the expression at position origin* */
  def toAdvString(orig: Int): String = {
    def toAdvStringRec(
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
        argList.map(toAdvStringRec(_, Nil, exprNames, forallPos))
      }

      exprNames get pos match {
        case Some(name) => combineHeadTail(name, argsToString(args))
        case None =>
          logicLayer.getExprForest.getExpr(pos) match {
            case Symbol(id) =>
              forallPos match {
                case Some(p) if p == pos =>
                  args match {
                    case Nil => combineHeadTail(generateName(id, numArgs), Nil)
                    case s :: xs =>
                      "{" + toAdvStringRec(
                        s,
                        Nil,
                        listToMap(argsToString(xs)),
                        None
                      ) + "}"
                  }
                case _ =>
                  combineHeadTail(generateName(id, numArgs), argsToString(args))
              }
            case Apply(next, arg) =>
              toAdvStringRec(next, arg :: args, exprNames, forallPos)
          }
      }
    }
    toAdvStringRec(orig, Nil, exprToString, Some(logicLayer.forallPos))
  }

  // ------------------------------------------
  // function to print a human readable proofs
  // ------------------------------------------

  // TODO: create a new class. Make it less String dependent

  /** list of expressions st way(0) -> way(1) -> ... -> way(n) * */
  def proofFromList(way: List[Int], alg: String): List[String] = {
    way match {
      case Nil     => Nil
      case List(a) => List(alg + toAdvString(a))
      case a :: b :: xs =>
        logicLayer.getInferenceOf(a, b) match {
          case None => Nil // todo: assert(false)
          case Some(ImplyIR(_, implyPos)) =>
            (alg + toAdvString(a)) :: proofFromPos(
              implyPos,
              alg + "|"
            ) ++ proofFromList(b :: xs, alg)
          case _ => (alg + toAdvString(a)) :: proofFromList(b :: xs, alg)
        }
    }
  }

  def proofFromPos(pos: Int, alg: String = ""): List[String] = {
    proofFromList(wayToTruth(pos), alg)
  }

  /** build list of expressions st way(0) -> way(1) -> ... -> way(n) * */
  def wayToTruth(orig: Int): List[Int] = {
    def wayToTruthRec(pos: Int): List[Int] =
      logicLayer.getTruthOriginOf(pos) match {
        case None          => List(pos)
        case Some(nextPos) => pos :: wayToTruthRec(nextPos)
      }

    logicLayer.getTruthOf(orig) match {
      case None          => Nil
      case Some(v) if v  => wayToTruthRec(orig).reverse
      case Some(v) if !v => wayToTruthRec(orig)
    }
  }

  /** build the way from true to false and create the proof * */
  def proofAbsurd: List[String] = logicLayer.getAbsurd match {
    case None => Nil
    case Some((a, b)) => {
      val way = logicLayer.getTruthOf(a) match {
        case None          => Nil // todo: assert(false)
        case Some(v) if v  => wayToTruth(a) ++ wayToTruth(b)
        case Some(v) if !v => wayToTruth(b) ++ wayToTruth(a)
      }
      proofFromList(way, "")
    }
  }

}
