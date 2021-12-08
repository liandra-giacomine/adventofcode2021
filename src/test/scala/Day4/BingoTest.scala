package Day4
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.ListBuffer
import scala.io.Source

class BingoTest extends AnyFunSuite {

  val numbers = Array(7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,19,25,20,12,22,18)
  val numbersExample = Array(7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1)
  val board1 = Array(Array(14 ,21, 17, 24,  4), Array(10 ,16, 15,  9, 19), Array(18, 8, 23, 26, 20), Array(22 ,11 ,13,  6,  5), Array(2 , 0, 12 , 3 , 7))
  val board2 = Array(Array(14 ,11, 7, 2,  8), Array(10 ,34, 6,  3, 20), Array(2, 8, 3, 46, 11), Array(22 ,3 ,56,  4,  5), Array(2 , 20, 2 , 6 , 7))
  val boards = Array(board1, board2)

  test("test bingo by checking marked rows") {
    assert(Bingo.getRowBingoIndexes(0, numbers, board1).min == 11)
  }

  test("test bingo by checking marked columns") {
    assert(Bingo.getColBingoIndexes(0, numbers, board1).min == 19)
  }

  test("calculate winner index") {
    assert(Bingo.getFirstWinnerBoardIndex(numbers, boards) == (0, 11))
  }

  test("get sum of unmarked numbers from the winning board") {
    assert(Bingo.sumUnmarkedNumbers( 11,numbersExample, board1) == 188)
  }

  val chosenNumbers = Array(15,61,32,33,87,17,56,73,27,83,0,18,43,8,86,37,40,6,93,25,14,68,64,57,39,46,55,13,21,72,51,81,78,79,52,65,36,92,97,28,9,24,22,69,70,42,3,4,63,50,91,16,41,94,77,85,49,12,76,67,11,62,99,54,95,1,74,34,88,89,82,48,84,98,58,44,5,53,7,19,29,30,35,26,31,10,60,59,80,71,45,38,20,66,47,2,23,96,90,75)
  val lines = Source.fromFile("Day4" ).getLines().toArray
  val list = new ListBuffer[Array[Array[Int]]]()

  var startIdx = 0
  var endIdx = 5
  while(endIdx <= lines.length) {
    val boardLines = lines.slice(startIdx, endIdx)
    boardLines.foreach(println)
    startIdx = endIdx + 1
    endIdx += 6
    val board = boardLines.map(line => line.split(" ").filter(!_.isEmpty).map(_.toInt))
    list += board
  }
  val challengeBoards = list.toArray

  test("get first winner score from file") {
    assert(Bingo.getFirstWinnerScore(chosenNumbers, challengeBoards) == 58412)
  }

  test("last board winner") {
    assert(Bingo.getLastWinnerScore(chosenNumbers, challengeBoards) == 10030)
  }

}
