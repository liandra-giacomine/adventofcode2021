package Day7

object WhaleTreachery {

  def calculateFuelConstantRate(startPositions: Array[Int], alignment: Int): Int = {
    startPositions.map(pos => Math.abs(pos - alignment)).sum
  }

  def calculateFuelChangingRate(startPositions: Array[Int], alignment: Int): Int = {
    startPositions.map(pos => (1 to Math.abs(pos - alignment)).sum).sum
  }

  def cheapestFuelCost(startPositions: Array[Int], maxAlignment: Int, constantRate: Boolean): Int = {
    val fuelCost = for (
      alignment <- 0 to maxAlignment;
      cost = if (constantRate) calculateFuelConstantRate(startPositions, alignment) else calculateFuelChangingRate(startPositions, alignment)
    ) yield cost

    fuelCost.min
  }


}
