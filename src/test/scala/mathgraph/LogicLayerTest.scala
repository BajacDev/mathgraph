package logiclayertest

import org.scalatest.funsuite.AnyFunSuite
import mathgraph.mathgraph._
import mathgraph.util.Pipe._

class LogicLayerTest extends AnyFunSuite {

  test("'true -> false' is absurd") {

    val absurd =
      new LogicLayer().init |> (ll => ll.setApply(ll.implyPos, ll.truePos)) |> {
        case (ll, pos) => ll.setApply(pos, ll.falsePos)
      } |> { case (ll, pos) =>
        ll.addTruth(pos, true)
      } |> (_.getAbsurd)

    assert(absurd)
  }

  test("'(false -> true) -> false' is absurd") {

    val absurd =
      new LogicLayer().init |> (ll =>
        ll.setApply(ll.implyPos, ll.falsePos)
      ) |> { case (ll, pos) => ll.setApply(pos, ll.truePos) } |> {
        case (ll, pos) => ll.setApply(ll.implyPos, pos)
      } |> { case (ll, pos) => ll.setApply(pos, ll.falsePos) } |> {
        case (ll, pos) => ll.addTruth(pos, true)
      } |> (_.getAbsurd)

    assert(absurd)
  }
}
