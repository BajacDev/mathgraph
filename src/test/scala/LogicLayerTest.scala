package logiclayertest

import org.scalatest.funsuite.AnyFunSuite
import mathgraph.mathgraph._
import mathgraph.util.Pipe._
import mathgraph.printer._

class LogicLayerTest extends AnyFunSuite {

  test("'true -> false' is absurd") {

    val absurd =
      new LogicLayer().init |> (ll => ll.setApply(ll.implyPos, ll.truePos)) |> {
        case (ll, pos) => ll.setApply(pos, ll.falsePos)
      } |> { case (ll, pos) =>
        ll.setAxiom(pos, true)
      } |> (_.isAbsurd)

    assert(absurd)
  }

  test("'(false -> true) -> false' is absurd") {

    val absurd =
      new LogicLayer().init |> (ll =>
        ll.setApply(ll.implyPos, ll.falsePos)
      ) |> { case (ll, pos) => ll.setApply(pos, ll.truePos) } |> {
        case (ll, pos) => ll.setApply(ll.implyPos, pos)
      } |> { case (ll, pos) => ll.setApply(pos, ll.falsePos) } |> {
        case (ll, pos) => ll.setAxiom(pos, true)
      } |> (_.isAbsurd)

    assert(absurd)
  }

  test("'{0(1, 2)}(->, true, false)' is absurd") {

    val absurd =
      new LogicLayer().init |> (ll =>
        ll.setApply(ll.idToPos(0), ll.idToPos(1))
      ) |> { case (ll, pos) => ll.setApply(pos, ll.idToPos(2)) } |> {
        case (ll, pos) => ll.setApply(ll.forallPos, pos)
      } |> { case (ll, pos) => ll.setApply(pos, ll.implyPos) } |> {
        case (ll, pos) => ll.setApply(pos, ll.truePos)
      } |> { case (ll, pos) => ll.setApply(pos, ll.falsePos) } |> {
        case (ll, pos) => ll.setAxiom(pos, true)
      } |> (_.isAbsurd)

    assert(absurd)
  }

}
