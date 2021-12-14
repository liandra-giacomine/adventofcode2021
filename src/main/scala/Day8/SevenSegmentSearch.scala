package Day8

import scala.io.Source

object SevenSegmentSearch extends App {

  val digitLetters = Map(0 -> "abcefg", 1 -> "cf", 2 -> "acdeg", 3 -> "acdfg", 4 -> "bcdf", 5 -> "abdfg", 6 -> "abdefg", 7 -> "acf", 8 -> "abcdefg", 9 -> "abcdfg")

  def getDecodedDigitSumFromFile(input: Array[String]): Int = {
    val numbers = for (
      line <- input;
      newLine = line.split(" ");
      inputSignal = newLine.slice(0, 10);
      letterMap = decodeSignal(inputSignal);
      outputSignal = newLine.slice(newLine.length - 4, newLine.length);
      num = decodeAndSumOutputSignal(outputSignal, letterMap)
    ) yield num

    println(numbers.sum)
    numbers.sum
  }

  def decodeAndSumOutputSignal(outputSignal: Array[String], letterMap: Map[Char, Char]): Int = {
    (for(
      signal <- outputSignal;
      digitFromLen = getDigitFromLen(signal);
      digit = if (digitFromLen > 0) digitFromLen else deriveDigitFromMap(signal, letterMap)
    ) yield digit).mkString.toInt
  }

  def deriveDigitFromMap(input: String, letterMap: Map[Char, Char]): Int = {

    val decoded = input.map(letter => letterMap.filter(_._2 == letter).head._1).sorted.mkString

    digitLetters.find(_._2 == decoded).head._1
  }

  val input = Source.fromFile("Day8" ).getLines().toArray

  getDecodedDigitSumFromFile(input)

  def decodeSignal(inputSignal: Array[String]): Map[Char, Char] = {
    val encodedOne = inputSignal.filter(x => x.length == 2)(0)
    val encodedFour = inputSignal.filter(x => x.length == 4)(0)
    val encodedSeven = inputSignal.filter(x => x.length == 3)(0)
    val encodedEight = inputSignal.filter(x => x.length == 7)(0)
    val a: Char = encodedSeven.filter(l => !encodedOne.contains(l))(0)

    println("ENCODED ONE:" + encodedOne)
    println("ENCODED SEVEN:" + encodedSeven)
    println("ENCODED EIGHT:" + encodedEight)
    println("a:" + a)

    val cf = encodedSeven.filter(l => encodedOne.contains(l))
    val sixLenSignals = inputSignal.filter(x => x.length == 6)
    val encodedSix: String = sixLenSignals.filter(signal => cf.filter(l => signal.contains(l)).length == 1)(0)

    val c: Char = encodedEight.filter(l => !encodedSix.contains(l))(0)
    val f: Char = cf.filter(l => l != c)(0)

    println("c:" + c)
    println("f:" + f)

    val nineAndZero = sixLenSignals.filter(signal => signal != encodedSix).map(_.sorted)

    val letterDiff1: Char = nineAndZero(0).filter(letter => !nineAndZero(1).contains(letter))(0)
    val letterDiff2: Char = nineAndZero(1).filter(letter => !nineAndZero(0).contains(letter))(0)

    val d = if (encodedFour.contains(letterDiff1)) letterDiff1 else letterDiff2
    val e = if (d == letterDiff1) letterDiff2 else letterDiff1

    val b = encodedFour.filter(l => l != d && l != c && l != f)(0)
    val g = encodedEight.filter(l => l != a && l != b && l != c && l != d && l != e && l != f)(0)

    println("b: " + b)
    println("d: " + d)
    println("e: " + e)
    println("g: " + g)

    Map('a' -> a,'b' -> b,'c' -> c,'d' -> d,'e' -> e,'f' -> f,'g' -> g)
  }

  def getUniqueDigitCount(input: Array[String]): Int = {
    val uniqueComboCount = for (
      line <- input;
      newLine = line.split(" ");
      output = newLine.slice(newLine.length - 4, newLine.length);
      lineCount = countUniqueDigits(output)
    ) yield lineCount

    uniqueComboCount.sum
  }

  def countUniqueDigits(input: Array[String]): Int = {
    input.map(value => getDigitFromLen(value)).filter(number => number == 1 || number == 4 || number == 7 || number == 8).length
  }

  def getDigitFromLen(input: String): Int = {
    input.length match {
      case 2 => 1
      case 3 => 7
      case 4 => 4
      case 7 => 8
      case _ => 0
    }
  }
}
