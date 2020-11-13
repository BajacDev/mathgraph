package logiclayertest

import org.scalatest.funsuite.AnyFunSuite
import mathgraph.corelogic._
import mathgraph.util.Pipe._

class LogicGraphTest extends AnyFunSuite {

  test("'true -> false' is absurd") {

    val absurd =
      LogicGraph.init |> (lg => lg.fix(lg.implyPos, lg.truePos)) |> {
        case (lg, pos) => lg.fix(pos, lg.falsePos)
      } |> { case (lg, pos) =>
        lg.setAxiom(pos, true)
      } |> (_.isAbsurd)

    assert(absurd)
  }

  test("'(false -> true) -> false' is absurd") {

    val absurd =
      LogicGraph.init |> (lg => lg.fix(lg.implyPos, lg.falsePos)) |> {
        case (lg, pos) => lg.fix(pos, lg.truePos)
      } |> { case (lg, pos) =>
        lg.fix(lg.implyPos, pos)
      } |> { case (lg, pos) => lg.fix(pos, lg.falsePos) } |> {
        case (lg, pos) => lg.setAxiom(pos, true)
      } |> (_.isAbsurd)

    assert(absurd)
  }

  test("'{0(1, 2)}(->, true, false)' is absurd") {

    val absurd =
      LogicGraph.init |> (lg => lg.fix(lg.idToPos(0), lg.idToPos(1))) |> {
        case (lg, pos) => lg.fix(pos, lg.idToPos(2))
      } |> { case (lg, pos) =>
        lg.fix(lg.forallPos, pos)
      } |> { case (lg, pos) => lg.fix(pos, lg.implyPos) } |> {
        case (lg, pos) => lg.fix(pos, lg.truePos)
      } |> { case (lg, pos) => lg.fix(pos, lg.falsePos) } |> {
        case (lg, pos) => lg.setAxiom(pos, true)
      } |> (_.isAbsurd)

    assert(absurd)
  }

}
