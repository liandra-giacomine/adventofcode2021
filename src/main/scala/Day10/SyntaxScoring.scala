package Day10
import scala.io.Source

object SyntaxScoring extends App {

  val bracketScore = Map(
    ')' -> 1,
    ']' -> 2,
    '}' -> 3,
    '>' -> 4
  )

  def illegalCharScore(lines: Array[String]): Int = {
    val illegalChars = lines.map(line => firstIllegalChar(line, 0, new Array[Char](0))).filterNot(_ == None)

    (illegalChars.count(_ == Some(')')) * 3) + (illegalChars.count(_ == Some(']')) * 57) + (illegalChars.count(_ == Some('}')) * 1197) + (illegalChars.count(_ == Some('>')) * 25137)
  }

  def middleScore(lines: Array[String]): Long = {
    val incompleteLines = lines.filter(line => firstIllegalChar(line, 0, new Array[Char](0)) == None)

    val incompleteBrackets = incompleteLines.map(line => completionString(line, 0, new Array[Char](0)))

    val sortedScores = incompleteBrackets.map(brackets => incompleteLineScore(brackets, 0, 0)).sorted

    val middleValIdx = ((sortedScores.length + 1) / 2) - 1

    sortedScores(middleValIdx)
  }

  val exampleInput = Source.fromFile("Day10" ).getLines().toArray
  println(middleScore(exampleInput))

  def incompleteLineScore(missingBrackets: Array[Char], idx: Int, score: Long): Long = {
    if(idx == missingBrackets.length) return score

    val total = (score * 5) + bracketScore(missingBrackets(idx))

    incompleteLineScore(missingBrackets, idx + 1, total)
  }


  def completionString(line: String, idx: Int, closingBrackets: Array[Char]): Array[Char] = {
    if(idx == line.length) {
      println(closingBrackets.mkString)
      return closingBrackets
    }

    line(idx) match {
      case '(' => completionString(line, idx + 1, ')' +: closingBrackets)
      case '[' => completionString(line, idx + 1, ']' +: closingBrackets)
      case '{' => completionString(line, idx + 1, '}' +: closingBrackets)
      case '<' => completionString(line, idx + 1, '>' +: closingBrackets)
      case x => {
        if (x == closingBrackets.head) {
          completionString(line, idx + 1, closingBrackets.tail)
        } else {
          completionString(line, idx + 1, closingBrackets)
        }
      }
    }
  }

  def firstIllegalChar(line: String, idx: Int, closingBrackets: Array[Char]): Option[Char] = {
    if(idx == line.length) return None

    line(idx) match {
      case '(' => firstIllegalChar(line, idx + 1, ')' +: closingBrackets)
      case '[' => firstIllegalChar(line, idx + 1, ']' +: closingBrackets)
      case '{' => firstIllegalChar(line, idx + 1, '}' +: closingBrackets)
      case '<' => firstIllegalChar(line, idx + 1, '>' +: closingBrackets)
      case x => {
        if (x == closingBrackets.head) {
          firstIllegalChar(line, idx + 1, closingBrackets.tail)
        } else {
          Some(x)
        }
      }
    }
  }

}
