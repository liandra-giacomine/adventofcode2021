package Day3

object PowerConsumption extends App {

  def getLifeSupRate(input: Array[String]): Int = {
    val oxygenRating = getOxygenRating(input)
    val CO2Rating = getC02Rating(input)

    oxygenRating * CO2Rating
  }

  def getOxygenRating(input: Array[String]): Int = {
    def oxygenRateHelper(index: Int, remaining: Array[String]): String = {
      if (remaining.length == 1) return remaining(0)

      val bit = mostCommonBit(index, remaining)
      val commonBits = remaining.filter(x => x(index) == bit)

      oxygenRateHelper(index + 1, commonBits)
    }

    Integer.parseInt(oxygenRateHelper(0, input), 2)
  }

  def getC02Rating(input: Array[String]): Int = {
    def C02RateHelper(index: Int, remaining: Array[String]): String = {
      if (remaining.length == 1) return remaining(0)

      val bit = leastCommonBit(index, remaining)
      val commonBits = remaining.filter(x => x(index) == bit)

      println(bit)
      commonBits.foreach(println)

      C02RateHelper(index + 1, commonBits)
    }

    Integer.parseInt(C02RateHelper(0, input), 2)
  }

  def calculatePowerConsumption(input: Array[String]): Int = {
    val gamma = calculateGamma(input)
    val epsilon = calculateEpsilon(gamma)

    Integer.parseInt(gamma, 2) * Integer.parseInt(epsilon, 2)
  }

  def calculateGamma(input: Array[String]): String = {
    if(input.length == 0) ""

    def gammaHelper(input: Array[String], index: Int, acc: String): String = {
      if(index == input(0).length) return acc

      gammaHelper(input, index + 1, acc + mostCommonBit(index, input))
    }
    gammaHelper(input, 0, "")
  }

  def calculateEpsilon(gamma: String): String = {
    def flipBit(value: Char): Char = {
      if (value == '1') '0' else '1'
    }

    gamma.map(x => flipBit(x)).mkString
  }

  def mostCommonBit(index: Int, input: Array[String]): Char = {
    val total = input.length
    val oneCount = input.count(x => x(index) == '1')
    val zeroCount = total - oneCount

   if(oneCount >= zeroCount) '1' else '0'
  }

  def leastCommonBit(index: Int, input: Array[String]): Char = {
    val total = input.length
    val oneCount = input.count(x => x(index) == '1')
    val zeroCount = total - oneCount

    if(zeroCount <= oneCount) '0' else '1'
  }

}
