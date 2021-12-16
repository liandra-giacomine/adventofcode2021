package Day9

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class SmokeBasinTest extends AnyFunSuite {

  val exampleInput = Source.fromFile("Day9Example" ).getLines().toArray.map(line => line.grouped(1).map(_.toInt).toArray)
  val challengeInput = Source.fromFile("Day9" ).getLines().toArray.map(line => line.grouped(1).map(_.toInt).toArray)

  test("lowest number to match example") {
    assert(SmokeBasin.getLowPointsSum(exampleInput) == 15)
  }

  test("lowest number in challenge input file") {
    assert(SmokeBasin.getLowPointsSum(challengeInput) == 494)
  }

  test("basin size top-left") {
    val coordinate = (0,1)
    assert(SmokeBasin.getBasinSize(coordinate, exampleInput) == 3)
  }

  test("basin size top-right") {
    val coordinate = (0,9)
    assert(SmokeBasin.getBasinSize(coordinate, exampleInput) == 9)
  }

  test("basin size middle") {
    val coordinate = (3,3)
    assert(SmokeBasin.getBasinSize(coordinate, exampleInput) == 14)
  }

  test("basin size bottom-right") {
    val coordinate = (4,6)
    assert(SmokeBasin.getBasinSize(coordinate, exampleInput) == 9)
  }

  test("multiple of largest three matches example") {
    assert(SmokeBasin.mulThreeLargestBasins( exampleInput) == 1134)
  }

  test("multiple of largest three in input file") {
    assert(SmokeBasin.mulThreeLargestBasins( challengeInput) == 1048128)
  }

}
