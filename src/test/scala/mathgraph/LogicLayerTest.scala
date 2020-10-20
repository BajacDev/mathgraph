package logiclayertest

import org.scalatest.funsuite.AnyFunSuite
import mathgraph.mathgraph._
import mathgraph.utils._

class LogicLayerTest extends AnyFunSuite {

  test("'true -> false' is absurd") {

    val logicLayer = Monad(new LogicLayer().init)
      .map(ll => ll.setApply(ll.implyPos, ll.truePos))
      .map { case (ll, pos) => ll.setApply(pos, ll.falsePos) }
      .map { case (ll, pos) => ll.addTruth(pos, true) }
      .get
    assert(logicLayer.getAbsurd)

  }

  test("'(false -> true) -> false' is absurd") {
    val logicLayer = Monad(new LogicLayer().init)
      .map(ll => ll.setApply(ll.implyPos, ll.falsePos))
      .map { case (ll, pos) => ll.setApply(pos, ll.truePos) }
      .map { case (ll, pos) => ll.setApply(ll.implyPos, pos) }
      .map { case (ll, pos) => ll.setApply(pos, ll.falsePos) }
      .map { case (ll, pos) => ll.addTruth(pos, true) }
      .get
    assert(logicLayer.getAbsurd)
  }
}
