package logiclayertest

import org.scalatest.funsuite.AnyFunSuite
import mathgraph.mathgraph._

class LogicLayerTest extends AnyFunSuite {

  test("'true -> false' is absurd") {
    // todo: make it less ugly
    val logicLayer = (((new LogicLayer().init match {
      case ll => ll.setApply(ll.implyPos, ll.truePos)
    }) match {
      case (ll, pos) => ll.setApply(pos, ll.falsePos)
    }) match {
      case (ll, pos) => ll.addTruth(pos, true)
    })
    assert(logicLayer.getAbsurd)
  }

  test("'(false -> true) -> false' is absurd") {
    // todo: make it less ugly
    val logicLayer = (((((new LogicLayer().init match {
      case ll => ll.setApply(ll.implyPos, ll.falsePos)
    }) match {
      case (ll, pos) => ll.setApply(pos, ll.truePos)
    }) match {
      case (ll, pos) => ll.setApply(ll.implyPos, pos)
    }) match {
      case (ll, pos) => ll.setApply(pos, ll.falsePos)
    }) match {
      case (ll, pos) => ll.addTruth(pos, true)
    })
    assert(logicLayer.getAbsurd)
  }
}
