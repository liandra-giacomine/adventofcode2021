package Day5

import org.scalatest.funsuite.AnyFunSuite

class HydrothermalVentureTest extends AnyFunSuite {

  val grid = Array.ofDim[Int](10, 10)
  val points = Array(Array((0,9), (5,9)), Array((8,0), (0,8)), Array((9,4), (3,4)), Array((2,2), (2,1)), Array((7,0), (7,4)), Array((6,4), (2,0)), Array((0,9), (2,9)), Array((3,4), (1,4)), Array((0,0), (8,8)),Array((5,5), (8,2)))

  test("set point in grid") {
    assert(HydrothermalVenture.setPointInGrid(grid, (2, 2), (2, 1), false)(2)(2) == 1)
  }

  test("set all points in grid") {
    val markedGrid = HydrothermalVenture.setAllPointsInGrid(grid, points, false)
    assert(markedGrid(0)(9) == 2)
    assert(markedGrid(1)(9) == 2)
    assert(markedGrid(2)(9) == 2)
  }

  test("matches example") {
    println(HydrothermalVenture.countTwoOrMoreOverlaps(grid, points, false) == 5)
  }

}
