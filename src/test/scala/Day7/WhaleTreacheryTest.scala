package Day7

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class WhaleTreacheryTest extends AnyFunSuite {

  val startPositions = Array(16,1,2,0,4,2,7,1,2,14)

  test("calculate fuel given alignment equals 2") {
    assert(WhaleTreachery.calculateFuelConstantRate(startPositions, 2) == 37)
  }

  test("calculate fuel given alignment equals 3") {
    assert(WhaleTreachery.calculateFuelConstantRate(startPositions, 3) == 39)
  }

  test("calculate cheapest fuel given all possible combinations") {
    assert(WhaleTreachery.cheapestFuelCost(startPositions, 10, true) == 37)
  }

  val inputstate = Source.fromFile("Day7" ).getLines().next().split(",").map(_.toInt)

  test("calculate cheapest fuel given file input") {
    assert(WhaleTreachery.cheapestFuelCost(inputstate, 2000, true) == 349357)
  }

  test("calculate fuel with changingRate") {
    assert(WhaleTreachery.calculateFuelChangingRate(startPositions, 5) == 168)
  }

  test("lowest fuel cost with changing rate to match example") {
    assert(WhaleTreachery.cheapestFuelCost(startPositions, 20, false) == 168)
  }

  test("lowest fuel cost from input file with changing rate") {
    assert(WhaleTreachery.cheapestFuelCost(inputstate, 2000, false) == 96708205)
  }
}

