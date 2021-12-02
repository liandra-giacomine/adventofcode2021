package Day1

object DepthIncrement extends App {

  def countDepthIncrements(input: Array[String]): Int = {

    var prev = -1
    var total = 0
    input.foreach { line =>
      val depth = line.toInt

      if (prev < 0) {
        prev = depth
      }

      if (depth > prev) {
        total = total + 1
      }

      prev = depth
    }
    total
  }

  def countSlidingWindowInc(lines: Array[String]): Int = {
    val lastIdx = lines.length - 3

    var idxStart = 0
    var idxEnd = 2
    var total = 0
    var prevDepth = -1
    var depth = 0
    while (idxStart <= lastIdx) {
      depth = Sum(lines.slice(idxStart, idxEnd + 1))

      if (prevDepth > 0) {
        if (depth > prevDepth) {
          total += 1
        }
      }

      idxStart += 1
      idxEnd += 1
      prevDepth = depth
    }

    total
  }


  def Sum(arr: Array[String]): Int = {
    var total = 0
    for (i <- arr) {
      total += i.toInt
    }
    total
  }

}
