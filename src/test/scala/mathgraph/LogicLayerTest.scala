package logiclayertest

import org.scalatest.funsuite.AnyFunSuite
import mathgraph.mathgraph._
import mathgraph.util.Pipe._

class LogicGraphTest extends AnyFunSuite {

  test("'true -> false' is absurd") {

    val absurd =
      new LogicGraph().init |> (lg => lg.apply(lg.implyPos, lg.truePos)) |> {
        case (lg, pos) => lg.apply(pos, lg.falsePos)
      } |> { case (lg, pos) =>
        lg.setAxiom(pos, true)
      } |> (_.isAbsurd)

    assert(absurd)
  }

  test("'(false -> true) -> false' is absurd") {

    val absurd =
      new LogicGraph().init |> (lg =>
        lg.apply(lg.implyPos, lg.falsePos)
      ) |> { case (lg, pos) => lg.apply(pos, lg.truePos) } |> {
        case (lg, pos) => lg.apply(lg.implyPos, pos)
      } |> { case (lg, pos) => lg.apply(pos, lg.falsePos) } |> {
        case (lg, pos) => lg.setAxiom(pos, true)
      } |> (_.isAbsurd)

    assert(absurd)
  }

  test("'{0(1, 2)}(->, true, false)' is absurd") {

    val absurd =
      new LogicGraph().init |> (lg =>
        lg.apply(lg.idToPos(0), lg.idToPos(1))
      ) |> { case (lg, pos) => lg.apply(pos,lg.idToPos(2)) } |> {
        case (lg, pos) => lg.apply(lg.forallPos, pos)
      } |> { case (lg, pos) => lg.apply(pos, lg.implyPos) } |> {
        case (lg, pos) => lg.apply(pos, lg.truePos)
      } |> { case (lg, pos) => lg.apply(pos, lg.falsePos) } |> {
        case (lg, pos) => lg.setAxiom(pos, true)
      } |> (_.isAbsurd)

    assert(absurd)
  }

}
