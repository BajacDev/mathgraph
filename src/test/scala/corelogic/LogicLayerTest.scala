package mathgraph.corelogic

import org.scalatest.funsuite.AnyFunSuite
import mathgraph.corelogic._
import mathgraph.util.Pipe._

import mathgraph.corelogic.ExprContainer._

class LogicGraphTest extends AnyFunSuite {

  test("'true -> false' is absurd") {

    val absurd =
      LogicGraph.init |> (lg => lg.fix(ImplySymbol, TrueSymbol)) |> {
        case (lg, pos) => lg.fix(pos, FalseSymbol)
      } |> { case (lg, pos) =>
        lg.setAxiom(pos, true)
      } |> (_.isAbsurd)

    assert(absurd)
  }

  test("'(false -> true) -> false' is absurd") {

    val absurd =
      LogicGraph.init |> (lg => lg.fix(ImplySymbol, FalseSymbol)) |> {
        case (lg, pos) => lg.fix(pos, TrueSymbol)
      } |> { case (lg, pos) =>
        lg.fix(ImplySymbol, pos)
      } |> { case (lg, pos) => lg.fix(pos, FalseSymbol) } |> { case (lg, pos) =>
        lg.setAxiom(pos, true)
      } |> (_.isAbsurd)

    assert(absurd)
  }

  test("'{0(1, 2)}(->, true, false)' is absurd") {

    val absurd =
      LogicGraph.init |> (lg => lg.fix(lg.idToSymbol(0), lg.idToSymbol(1))) |> {
        case (lg, pos) => lg.fix(pos, lg.idToSymbol(2))
      } |> { case (lg, pos) =>
        lg.fix(ForallSymbol, pos)
      } |> { case (lg, pos) => lg.fix(pos, ImplySymbol) } |> { case (lg, pos) =>
        lg.fix(pos, TrueSymbol)
      } |> { case (lg, pos) => lg.fix(pos, FalseSymbol) } |> { case (lg, pos) =>
        lg.setAxiom(pos, true)
      } |> (_.isAbsurd)

    assert(absurd)
  }

}
