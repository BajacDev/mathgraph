package logiclayertest

import org.scalatest.FunSuite

class LogicLayerTest extends FunSuite {
  
  test("LogicLayerTest") {
      val logicLayer = new LogicLayer.init
      val pos, logicLayer2 = logicLayer.setApply(logicLayer.truePos, logicLayer.falsePos)
      val logicLayer3 = logicLayer2.setTruth(pos, true)
      assert(logicLayer3.absurd)
  }
}
