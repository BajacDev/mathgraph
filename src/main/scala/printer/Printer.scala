package mathgraph.printer

import mathgraph.mathgraph._

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

  def toSimpleString(pos: Int): String = {
    // todo find a better way to access this function (too much dots)
    logicLayer.getExprLayer.getHeadTail(pos) match {
      case (Symbol(id), Seq()) => id.toString
      case (Symbol(id), seq) =>
        id.toString + "(" + seq.map(toSimpleString).mkString(", ") + ")"
    }
  }

  // ------------------------------------------
  // function to print proofs
  // ------------------------------------------

  /** list of expressions st way(0) -> way(1) -> ... -> way(n) * */
  def proofFromList(way: List[Int], alg: String): List[String] = {
    way match {
      case Nil     => Nil
      case List(a) => List(alg + toSimpleString(a))
      case a :: b :: xs =>
        logicLayer.getInferenceOf(a, b) match {
          case None => Nil // todo: assert(false)
          case Some(ImplyIR(_, implyPos)) =>
            (alg + toSimpleString(a)) :: proofFromPos(
              implyPos,
              alg + "|"
            ) ++ proofFromList(b :: xs, alg)
          case _ => (alg + toSimpleString(a)) :: proofFromList(b :: xs, alg)
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
