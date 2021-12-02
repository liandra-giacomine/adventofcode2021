package Day1
import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class DepthIncrementTest extends AnyFunSuite {
  test("no increments") {
    assert(DepthIncrement.countDepthIncrements(Array("176", "166","155")) == 0)
  }

  test("one increment") {
    assert( DepthIncrement.countDepthIncrements(Array("156","176","175")) == 1)
  }

  test("a few increments") {
    assert(DepthIncrement.countDepthIncrements(Array("196","198","202","203","187","189","194","213","216","217")) == 8)
  }

  test("given example matches expected increment 7") {
    assert(DepthIncrement.countDepthIncrements(Array("199","200","208","210","200","207","240","269","260","263")) == 7)
  }

  test("get depth increments from input file") {
    assert(DepthIncrement.countDepthIncrements(Source.fromFile("Day1" ).getLines().toArray) == 1393)
  }

  test("sliding window given example matches expected increment 5") {
    assert(DepthIncrement.countSlidingWindowInc(Array("199","200","208","210","200","207","240","269","260","263")) == 5)
  }

  test("get sliding window depth increments from input file") {
    assert(DepthIncrement.countSlidingWindowInc(Source.fromFile("Day1" ).getLines().toArray) == 1359)
  }

}
