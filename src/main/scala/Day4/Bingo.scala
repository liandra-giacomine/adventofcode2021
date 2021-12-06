package Day4
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Bingo extends App {

  def getFirstWinnerScore(numbers: Array[Int], boards: Array[Array[Array[Int]]]): Int = {
    val (boardIdx, bingoIdx) = getFirstWinnerBoardIndex(numbers, boards)
    val board = boards(boardIdx)
    val sum = sumUnmarkedNumbers(bingoIdx, numbers, board)
    numbers(bingoIdx) * sum
  }

  def getLastWinnerScore(numbers: Array[Int], boards: Array[Array[Array[Int]]]): Int = {
    val (boardIdx, bingoIdx) = getLastWinnerBoardIndex(numbers, boards)
    val board = boards(boardIdx)
    val sum = sumUnmarkedNumbers(bingoIdx, numbers, board)
    numbers(bingoIdx) * sum
  }

  def getFirstWinnerBoardIndex(numbers: Array[Int], boards: Array[Array[Array[Int]]]): (Int, Int) = {
    var i = 0
    var minBingoIdx = Int.MaxValue
    var winnerIdx = -1
    for (b <- boards) {
      val bingoIdx =  checkBoard(numbers, b)
      if(bingoIdx >= 0 && bingoIdx < minBingoIdx) {
        minBingoIdx = bingoIdx
        winnerIdx = i
      }
      i += 1
    }

    (winnerIdx, minBingoIdx)
  }

  def getLastWinnerBoardIndex(numbers: Array[Int], boards: Array[Array[Array[Int]]]): (Int, Int) = {
    var i = 0
    var maxBingoIdx = Int.MinValue
    var winnerIdx = -1
    for (b <- boards) {
      val bingoIdx = checkBoard(numbers, b)
      if(bingoIdx >= 0 && bingoIdx > maxBingoIdx) {
        maxBingoIdx = bingoIdx
        winnerIdx = i
      }
      i += 1
    }

    (winnerIdx, maxBingoIdx)
  }

  def checkBoard(numbers: Array[Int], board: Array[Array[Int]]): Int = {
    val rowList = getRowBingoIndexes(0, numbers, board)
    var bingoRowIdx = -1
    if(!rowList.isEmpty) {
      bingoRowIdx = rowList.min
    }
    val colList = getColBingoIndexes(0, numbers, board)
    var bingoColIdx = -1
    if(!colList.isEmpty) {
      bingoColIdx = colList.min
    }
    if(bingoRowIdx < 0) return bingoColIdx
    if(bingoColIdx < 0) return bingoRowIdx

    if(bingoRowIdx < bingoColIdx) bingoRowIdx else bingoColIdx
  }

  def getRowBingoIndexes(rowIndex: Int, numbers: Array[Int], board: Array[Array[Int]], list: ListBuffer[Int] = new ListBuffer[Int]): ListBuffer[Int] = {
    if(rowIndex == board.length) return list

    val bingo = for (
      col <- 0 to board.length - 1
      if(numbers.contains(board(rowIndex)(col)))
    ) yield board(rowIndex)(col)

    if (bingo.length == board.length) {
      list += bingo.map(num => numbers.indexOf(num)).max
    }

    getRowBingoIndexes(rowIndex + 1, numbers, board, list)
  }

  def getColBingoIndexes(colIndex: Int, numbers: Array[Int], board: Array[Array[Int]], list: ListBuffer[Int] = new ListBuffer[Int]): ListBuffer[Int]= {
    if(colIndex == board.length) return list

    val bingo = for (
      row <- 0 to board.length - 1
      if(numbers.contains(board(row)(colIndex)))
    ) yield board(row)(colIndex)

    if (bingo.length == board.length) {
      list += bingo.map(num => numbers.indexOf(num)).max
    }

    getColBingoIndexes(colIndex + 1, numbers, board, list)
  }

  def sumUnmarkedNumbers(lastIdx: Int, numbers: Array[Int], board: Array[Array[Int]]): Int = {
    val prevBingoNumbers = numbers.filter(num => numbers.indexOf(num) <= lastIdx)
    val unmarked = for(
      row <- 0 to board.length - 1;
      col <- 0 to board.length - 1;
      if(!prevBingoNumbers.contains(board(row)(col)))
    ) yield board(row)(col)

    unmarked.sum
  }

}
