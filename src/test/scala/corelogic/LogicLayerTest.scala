package mathgraph.corelogic

import org.scalatest.funsuite.AnyFunSuite
import mathgraph.corelogic._
import mathgraph.util.Pipe._

import mathgraph.corelogic.ExprContainer._

class LogicLayerTest extends AnyFunSuite {

  test("'true -> false' is absurd") {

    val lg = LogicGraph.init
    var pos = lg.fix(ImplySymbol, TrueSymbol)
    pos = lg.fix(pos, FalseSymbol)
    lg.setAxiom(pos, true)

    assert(lg.isAbsurd)
  }

  test("'(false -> true) -> false' is absurd") {

    val lg = LogicGraph.init
    var pos = lg.fix(ImplySymbol, FalseSymbol)
    pos = lg.fix(pos, TrueSymbol)
    pos = lg.fix(ImplySymbol, pos)
    pos = lg.fix(pos, FalseSymbol)
    lg.setAxiom(pos, true)

    assert(lg.isAbsurd)
  }

  test("'{0(1, 2)}(->, true, false)' is absurd") {

    val lg = LogicGraph.init
    var pos = lg.fix(lg.idToSymbol(0), lg.idToSymbol(1))
    pos = lg.fix(pos, lg.idToSymbol(2))
    pos = lg.fix(ForallSymbol, pos)
    pos = lg.fix(pos, ImplySymbol)
    pos = lg.fix(pos, TrueSymbol)
    pos = lg.fix(pos, FalseSymbol)
    lg.setAxiom(pos, true)

    assert(lg.isAbsurd)
  }

}
