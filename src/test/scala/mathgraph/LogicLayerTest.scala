package logiclayertest

import org.scalatest.funsuite.AnyFunSuite
import mathgraph.mathgraph._

class LogicLayerTest extends AnyFunSuite {
  
  test("true -> false") {
    // todo: make it less ugly
      val logicLayer1 = new LogicLayer().init
      val (pos2, logicLayer2) = logicLayer1.setApply(logicLayer1.implyPos, logicLayer1.truePos)
      val (pos3, logicLayer3) = logicLayer2.setApply(pos2, logicLayer2.falsePos)
      val logicLayer4 = logicLayer3.addTruth(pos3, true)
      assert(logicLayer4.getAbsurd)
  }
}
