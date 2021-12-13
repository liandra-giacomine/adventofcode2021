package Day8

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class SevenSegmentSearchTest extends AnyFunSuite {

  val inputstate = Source.fromFile("Day7" ).getLines().next().split(",").map(_.toInt)

  test("sum of unique segment size digits") {
    assert(SevenSegmentSearch.getOutputDigits())
  }

  test("sum of unique segment size digits") {
    assert(SevenSegmentSearch.countUniqueDigits())
  }

}
