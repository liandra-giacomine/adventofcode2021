package Day10

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class SyntaxScoringTest extends AnyFunSuite{

  test("finds illegal character ]") {
    val line = "{([(<{}[<>[]}>{[]{[(<()>"
    assert(SyntaxScoring.firstIllegalChar(line, 0, new Array[Char](0)) == Some('}'))
  }

  test("finds illegal character )") {
    val line = "[[<[([]))<([[{}[[()]]]"
    assert(SyntaxScoring.firstIllegalChar(line,0, new Array[Char](0)) == Some(')'))
  }

  test("finds illegal character >") {
    val line = "<{([([[(<>()){}]>(<<{{"
    assert(SyntaxScoring.firstIllegalChar(line,0, new Array[Char](0)) == Some('>'))
  }

  test("no illegal character in line") {
    val line = "[({(<(())[]>[[{[]{<()<>>"
    assert(SyntaxScoring.firstIllegalChar(line,0, new Array[Char](0)) == None)
  }

  val exampleInput = Source.fromFile("Day10Example" ).getLines().toArray

  test("sum score of illegal chars matches example") {
    assert(SyntaxScoring.illegalCharScore(exampleInput) == 26397)
  }

  val challengeInput = Source.fromFile("Day10" ).getLines().toArray

  test("sum score of illegal chars in input file") {
    assert(SyntaxScoring.illegalCharScore(challengeInput) == 311949)
  }

  test("completion string example 1") {
    val line = "<{([{{}}[<[[[<>{}]]]>[]]"
    assert(SyntaxScoring.completionString(line, 0, new Array[Char](0)).sameElements(Array(']', ')', '}', '>')))
  }

  test("completion string example 2") {
    val line = "[({(<(())[]>[[{[]{<()<>>"
    assert(SyntaxScoring.completionString(line, 0, new Array[Char](0)).sameElements(Array('}', '}', ']', ']', ')', '}', ')', ']')))
  }

  test("get incomplete line score") {
    val missingBrackets = Array(']', ')', '}', '>')
    assert(SyntaxScoring.incompleteLineScore(missingBrackets, 0, 0) == 1)
  }

}
