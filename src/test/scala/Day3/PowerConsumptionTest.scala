package Day3
import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class PowerConsumptionTest extends AnyFunSuite {

  test("calculate first most common bit") {
    assert(PowerConsumption.mostCommonBit(0, Array("11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","010100")) == '1')
  }

  test("calculate second most common bit") {
    assert(PowerConsumption.mostCommonBit(1, Array("11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","010100")) == '0')
  }

  test("calculate third most common bit") {
    assert(PowerConsumption.mostCommonBit(2, Array("11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","010100")) == '1')
  }

  test("calculate fourth most common bit") {
    assert(PowerConsumption.mostCommonBit(3, Array("11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","010100")) == '1')
  }

  test("calculate fifth most common bit") {
    assert(PowerConsumption.mostCommonBit(4, Array("11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","010100")) == '0')
  }

  test("calculate gamma rate") {
    assert(PowerConsumption.calculateGamma(Array("11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","010100")) == "10110")
  }

  test("calculate epsilon rate") {
    assert(PowerConsumption.calculateEpsilon("10110") == "01001")
  }

  test("power consumption matches example") {
    assert(PowerConsumption.calculatePowerConsumption(Array("11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","01010")) == 198)
  }

  test("calculate power consumption in file") {
    assert(PowerConsumption.calculatePowerConsumption(Source.fromFile("Day3" ).getLines().toArray) == 4103154)
  }

  test("oxygen rating with 1 value") {
    assert(PowerConsumption.getOxygenRating(Array("11110")) == 30)
  }

  test("oxygen rating with various values") {
    assert(PowerConsumption.getOxygenRating(Array("01101","11101","10101")) == 29)
  }

  test("oxygen rating matches example") {
    assert(PowerConsumption.getOxygenRating(Array("00100","11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","010100")) == 23)
  }

  test("cO2 rating with one value") {
    assert(PowerConsumption.getC02Rating(Array("11110")) == 30)
  }

  test("cO2 rating with various values") {
    assert(PowerConsumption.getC02Rating(Array("01101","11101","10101")) == 13)
  }

  test("CO2 rating matches example") {
    assert(PowerConsumption.getC02Rating(Array("00100","11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","01010")) == 10)
  }

  test("get life supporting rate of example") {
    assert(PowerConsumption.getLifeSupRate(Array("00100","11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","01010")) == 230)
  }

  test("get life supporting rate in input file") {
    assert(PowerConsumption.getLifeSupRate(Source.fromFile("Day3" ).getLines().toArray) == 4245351)
  }
}
