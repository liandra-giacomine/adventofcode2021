package Day6

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class LanternfishTest extends AnyFunSuite {

  val initialState = Array(3,4,3,1,2)
  test("initial state") {
    assert(Lanternfish.countFishArray(initialState, 0) == 5)
  }

  test("day 1") {
    assert(Lanternfish.countFishArray(initialState, 1) == 5)
  }

  test("day 2") {
    assert(Lanternfish.countFishArray(initialState, 2) == 6)
  }

  test("day 3") {
    assert(Lanternfish.countFishArray(initialState, 3) == 7)
  }

  test("day 18") {
    assert(Lanternfish.countFishArray(initialState, 18) == 26)
  }

  test("day 80") {
    assert(Lanternfish.countFishArray(initialState, 80) == 5934)
  }

  val inputstate = Source.fromFile("Day6" ).getLines().next().split(",").map(_.toInt)

  test("count after 80 days with input file initial state") {
    assert(Lanternfish.countFishArray(inputstate, 80) == 385391)
  }

  test("initial state efficient func") {
    assert(Lanternfish.countFishRec(initialState, 18, 0, 0) == 26)
  }

  test("coount fish 2") {
    assert(Lanternfish.countFishRec(initialState, 18, 0, 0) == 26)
  }

  test("coount fish 80") {
    assert(Lanternfish.countFishRec(initialState, 80, 0, 0) == 5934)
  }


}
