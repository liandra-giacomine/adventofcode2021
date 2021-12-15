package Day9

import Day9.SmokeBasin.adjDownValue

import scala.io.Source

object SmokeBasin extends App {

  def getLowPointsSum(heightmap: Array[Array[Int]]): Int = {
    var sum = 0
    for(col <- 0 until heightmap(0).length) {
      val lowPointCoordinates = getLowPointCoordinates(heightmap, 0, col, new Array[(Int, Int)](0))
      if (!lowPointCoordinates.isEmpty) {

        sum += lowPointCoordinates.map(c => heightmap(c._1)(c._2) + 1).sum
      }
    }
    sum
  }

  def mulThreeLargestBasins(heightmap: Array[Array[Int]]): Int = {

    def getAllLowPoints(col: Int, acc: Array[(Int,Int)]): Array[(Int,Int)] = {
      if(col == heightmap(0).length) return acc

      getAllLowPoints(col + 1, acc ++ getLowPointCoordinates(heightmap, 0, col, new Array[(Int, Int)](0)))
    }

    val allLowPoints = getAllLowPoints(0, new Array[(Int, Int)](0))

    val basinSizes = allLowPoints.map(c => getBasinSize(c, heightmap)).sorted.slice(allLowPoints.length - 3, allLowPoints.length)

    basinSizes(0) * basinSizes(1) * basinSizes(2)
  }


  def getBasinSize(lowestPoint: (Int, Int), heightmap: Array[Array[Int]]): Int = {

    def getBasinSizeRec(adjCoordinates: Array[(Int, Int)], basinCoordinates: Array[(Int, Int)]): Int = {
      if(adjCoordinates.isEmpty) return basinCoordinates.length

      val coordinate = adjCoordinates.head

      val adjToCurrent = getAdjacentCoordinates(coordinate, heightmap).filterNot(c => heightmap(c._1)(c._2) == 9 || basinCoordinates.contains(c))

      val nextAdjCoordinates = (adjCoordinates.filterNot(_ == coordinate) ++ adjToCurrent)

      getBasinSizeRec(nextAdjCoordinates, basinCoordinates :+ coordinate)
    }

    val adjToLowest = getAdjacentCoordinates(lowestPoint, heightmap).filterNot(c => heightmap(c._1)(c._2) == 9)
    getBasinSizeRec(adjToLowest, Array(lowestPoint))
  }


  def getAdjacentCoordinates(coordinate: (Int, Int), heightmap: Array[Array[Int]]): Array[(Int, Int)] = {

    val row = coordinate._1
    val col = coordinate._2

    if(row == 0) {
      if (col == 0) {
        //first row, first column
        Array(adjDownCoord(row, col), adjRightCoord(row, col))
      } else if (col == heightmap(0).length - 1) {
        //first row, last column
        Array(adjDownCoord(row, col), adjLeftCoord(row, col))
       } else {
        //first row, remaining columns
        Array(adjDownCoord(row, col), adjLeftCoord(row, col), adjRightCoord(row, col))
      }
    } else if (row == heightmap.length - 1) {
      if (col == 0) {
        //last row, first column
        Array(adjUpCoord(row, col), adjRightCoord(row, col))
       } else if (col == heightmap(0).length - 1) {
        //last row, last column
        Array(adjUpCoord(row, col), adjLeftCoord(row, col))
      } else {
        //last row, remaining columns
        Array(adjUpCoord(row, col), adjLeftCoord(row, col), adjRightCoord(row, col))
      }
    } else {
      if (col == 0) {
        //last row, first column
        Array(adjUpCoord(row, col), adjDownCoord(row, col), adjRightCoord(row, col))
      } else if (col == heightmap(0).length - 1) {
        //last row, last column
        Array(adjUpCoord(row, col), adjDownCoord(row, col), adjLeftCoord(row, col))
    } else {
        //last row, remaining columns
        Array(adjUpCoord(row, col), adjDownCoord(row, col), adjRightCoord(row, col), adjLeftCoord(row, col))
      }
    }
  }

  def adjRightCoord(row: Int, col: Int) = (row, col + 1)
  def adjLeftCoord(row: Int, col: Int) = (row, col - 1)
  def adjUpCoord(row: Int, col: Int) = (row - 1, col)
  def adjDownCoord(row: Int, col: Int) = (row + 1, col)

  def adjRightValue(heightmap: Array[Array[Int]], row: Int, col: Int): Int = heightmap(row)(col + 1)
  def adjLeftValue(heightmap: Array[Array[Int]], row: Int, col: Int): Int = heightmap(row)(col - 1)
  def adjUpValue(heightmap: Array[Array[Int]], row: Int, col: Int): Int = heightmap(row - 1)(col)
  def adjDownValue(heightmap: Array[Array[Int]], row: Int, col: Int): Int = heightmap(row + 1)(col)

  def getLowPointCoordinates(heightmap: Array[Array[Int]], row: Int, col: Int, acc: Array[(Int, Int)]): Array[(Int, Int)] = {
    if (row == heightmap.length) return acc

    val cur: Int = heightmap(row)(col)

    if(row == 0) {
      if (col == 0) {
        //first row, first column
        if(cur < adjDownValue(heightmap, row, col) && cur < adjRightValue(heightmap, row, col)) getLowPointCoordinates(heightmap, row + 1, col, acc :+ (row, col)) else getLowPointCoordinates(heightmap, row + 1, col, acc)
      } else if (col == heightmap(0).length - 1) {
        //first row, last column
        if(cur < adjDownValue(heightmap, row, col) && cur < adjLeftValue(heightmap, row, col)) getLowPointCoordinates(heightmap, row + 1, col, acc :+ (row, col)) else getLowPointCoordinates(heightmap, row + 1, col, acc)
      } else {
        //first row, remaining columns
        if(cur < adjDownValue(heightmap, row, col) && cur < adjLeftValue(heightmap, row, col) && cur < adjRightValue(heightmap, row, col)) getLowPointCoordinates(heightmap, row + 1, col, acc :+ (row, col)) else getLowPointCoordinates(heightmap, row + 1, col, acc)
      }
    } else if (row == heightmap.length - 1) {
      if (col == 0) {
        //last row, first column
        if (cur < adjUpValue(heightmap, row, col) && cur < adjRightValue(heightmap, row, col)) getLowPointCoordinates(heightmap, row + 1, col, acc :+ (row, col)) else getLowPointCoordinates(heightmap, row + 1, col, acc)
      } else if (col == heightmap(0).length - 1) {
        //last row, last column
        if (cur < adjUpValue(heightmap, row, col) && cur < adjLeftValue(heightmap, row, col)) getLowPointCoordinates(heightmap, row + 1, col, acc :+ (row, col)) else getLowPointCoordinates(heightmap, row + 1, col, acc)
      } else {
        //last row, remaining columns
        if (cur < adjUpValue(heightmap, row, col) && cur < adjLeftValue(heightmap, row, col) && cur < adjRightValue(heightmap, row, col)) getLowPointCoordinates(heightmap, row + 1, col, acc :+ (row, col)) else getLowPointCoordinates(heightmap, row + 1, col, acc)
      }
    } else {
      if (col == 0) {
        //last row, first column
        if (cur < adjUpValue(heightmap, row, col) && cur < adjDownValue(heightmap, row, col) && cur < adjRightValue(heightmap, row, col)) getLowPointCoordinates(heightmap, row + 1, col, acc :+ (row, col)) else getLowPointCoordinates(heightmap, row + 1, col, acc)
      } else if (col == heightmap(0).length - 1) {
        //last row, last column
        if (cur < adjUpValue(heightmap, row, col) && cur < adjDownValue(heightmap, row, col) && cur < adjLeftValue(heightmap, row, col)) getLowPointCoordinates(heightmap, row + 1, col, acc :+ (row, col)) else getLowPointCoordinates(heightmap, row + 1, col, acc)
      } else {
        //last row, remaining columns
        if (cur < adjUpValue(heightmap, row, col) && cur < adjDownValue(heightmap, row, col) && cur < adjLeftValue(heightmap, row, col) && cur < adjRightValue(heightmap, row, col)) getLowPointCoordinates(heightmap, row + 1, col, acc :+ (row, col)) else getLowPointCoordinates(heightmap, row + 1, col, acc)
      }
    }
  }

}
