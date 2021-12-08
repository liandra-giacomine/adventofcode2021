package Day5

import scala.collection.mutable.ListBuffer
import scala.io.Source

object HydrothermalVenture extends App {

  def countTwoOrMoreOverlaps(grid: Array[Array[Int]], points:  Array[Array[(Int, Int)]], diagonal: Boolean): Int = {
    val markedGrid = setAllPointsInGrid(grid, points, diagonal)

    (for (row <- markedGrid.indices;
         col <- markedGrid.indices;
         if (markedGrid(row)(col) >= 2))
    yield markedGrid(row)(col)).length
  }

  def setAllPointsInGrid(grid: Array[Array[Int]], points:  Array[Array[(Int, Int)]], diagonal: Boolean): Array[Array[Int]] = {
    def traversePoints(i: Int, markedGrid: Array[Array[Int]]): Array[Array[Int]] = {
      if (i >= points.length) return markedGrid
      val newGrid = setPointInGrid(grid, points(i)(0), points(i)(1), diagonal)
      traversePoints(i + 1, newGrid)
    }

    traversePoints(0, grid)
  }

  def setPointInGrid(grid: Array[Array[Int]], cord1: (Int, Int), cord2: (Int, Int), diagonal: Boolean): Array[Array[Int]] = {

    if (cord1._1 == cord2._1) {
        val start = if (cord1._2 > cord2._2) cord2._2 else cord1._2
        val end = if (start == cord1._2) cord2._2 else cord1._2

        val x = cord1._1
        (start to end).map(y => grid(x)(y) += 1)
      } else if (cord1._2 == cord2._2) {
        val start = if (cord1._1 > cord2._1) cord2._1 else cord1._1
        val end = if (start == cord1._1) cord2._1 else cord1._1

        val y = cord1._2
        (start to end).map(x => grid(x)(y) += 1)
    } else if(diagonal) {

      val startX =  if(cord1._1 < cord2._1) cord1._1 else cord2._1
      val startY = if(startX == cord1._1) cord1._2 else cord2._2
      val endX = if(startX == cord1._1) cord2._1 else cord1._1
      val endY = if(startX == cord1._1) cord2._2 else cord1._2

      val increaseY = startY < endY

      if(increaseY) {
       markDiagonalPoint(startX, startY, endX, endY, grid, increaseY)
      } else {
        markDiagonalPoint(startX, startY, endX, endY, grid, increaseY)
      }
    }

    grid
  }

  def markDiagonalPoint(x: Int, y: Int, endX: Int, endY:Int, grid: Array[Array[Int]], increaseY: Boolean): Unit = {
    if(x == endX && y == endY) {
      grid(x)(y) += 1
      return grid
    }

    grid(x)(y) += 1

    increaseY match {
      case true => markDiagonalPoint(x + 1, y + 1, endX, endY, grid, increaseY)
      case false => markDiagonalPoint(x + 1, y - 1, endX, endY, grid, increaseY)
    }
  }


  val grid = Array.ofDim[Int](1000, 1000)
  val points = new ListBuffer[Array[(Int, Int)]]()
  //val points = Array(Array((0,9), (5,9)), Array((8,0), (0,8)), Array((9,4), (3,4)), Array((2,2), (2,1)), Array((7,0), (7,4)), Array((6,4), (2,0)), Array((0,9), (2,9)), Array((3,4), (1,4)), Array((0,0), (8,8)),Array((5,5), (8,2)))
  val lines = Source.fromFile("Day5" ).getLines().toArray

  def getAnswerFromFile(): Unit = {
    for (line <- lines) {
      val splitValues = line.replace(" -> ",",").split(",")
      val point1 = (splitValues(0).toInt, splitValues(1).toInt)
      val point2 = (splitValues(2).toInt, splitValues(3).toInt)
      val arr = new Array[(Int, Int)](2)
      arr(0) = point1
      arr(1) = point2
      points += arr
      }

    countTwoOrMoreOverlaps(grid, points.toArray, false)

    countTwoOrMoreOverlaps(grid, points.toArray, true)
    }

  getAnswerFromFile()

}
