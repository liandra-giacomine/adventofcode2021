package Day6

object Lanternfish extends App {

  def countFishArray(initialState: Array[Int], lastDay: Int): Int = {
    def nextDayRecursion(curState: Array[Int], day: Int): Int = {
      if(day == lastDay) return curState.length

      val stateSubDays = curState.map(day => if(day > 0) day - 1 else 6)
      val addDays = Array.fill[Int](curState.filter(x => x == 0).length)(8)

      val newState = stateSubDays ++ addDays

      nextDayRecursion(newState, day + 1)
    }
    nextDayRecursion(initialState, 0)
  }


  def countFishRec(state: Array[Int], lastDay: Int, idx: Int, acc: Int): Int = {
    if(idx == state.length) return acc

    val initialDay = state(idx)
    val newFishCount = calculateNewFishCreationDay(initialDay, 0, lastDay, 1)
    countFishRec(state, lastDay, idx + 1, acc + newFishCount)
  }

  def calculateNewFishCreationDay(timer: Int, day: Int, lastDay: Int, acc: Int): Int = {
    if(day == lastDay) return acc

    if(timer == 0) {
      calculateNewFishCreationDay(6, day + 1, lastDay, acc + 1) + calculateNewFishCreationDay(8, (day + 1), lastDay, 0)
    } else {
      calculateNewFishCreationDay(timer - 1, day + 1, lastDay, acc)
    }
  }

}