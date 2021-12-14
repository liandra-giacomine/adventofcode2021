package Day8

import Day8.SevenSegmentSearch.getDecodedDigitSumFromFile
import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class SevenSegmentSearchTest extends AnyFunSuite {

  val exampleInput = Source.fromFile("Day8Example" ).getLines().toArray

  test("should return digit 1") {
    assert(SevenSegmentSearch.getDigitFromLen("gc") == 1)
  }

  test("should return digit 4") {
    assert(SevenSegmentSearch.getDigitFromLen("gcbe") == 4)
  }

  test("should return digit 7") {
    assert(SevenSegmentSearch.getDigitFromLen("cgb") == 7)
  }

  test("should return digit 8") {
    assert(SevenSegmentSearch.getDigitFromLen("gbcadfe") == 8)
  }

  test("sum of unique segment size digits matches example") {
    assert(SevenSegmentSearch.getUniqueDigitCount(exampleInput) == 26)
  }

  val challengeInput = Source.fromFile("Day8" ).getLines().toArray

  test("sum of unique segment size digits for challenge") {
    assert(SevenSegmentSearch.getUniqueDigitCount(challengeInput) == 440)
  }


}
