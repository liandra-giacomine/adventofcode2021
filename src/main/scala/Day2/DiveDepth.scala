package Day2

object DiveDepth {

  def calculateDepth(input :Array[String]): Int = {

    var horizontal = 0
    var depth = 0
    input.foreach { line =>
      val posDepth = line.split(" ")
      val direction = posDepth(0)
      val count = posDepth(1).toInt

      direction match {
        case "forward" => (horizontal += count)
        case "up" => (depth -= count)
        case "down" => (depth += count)
      }
    }
    horizontal * depth
  }

  def calculateDepthWithAim(input :Array[String]): Int = {

    var horizontal = 0
    var depth = 0
    var aim = 0
    input.foreach { line =>
      val posDepth = line.split(" ")
      val direction = posDepth(0)
      val count = posDepth(1).toInt

      direction match {
        case "forward" => {
          horizontal += count
          depth += (count * aim)
        }
        case "up" => (aim -= count)
        case "down" => (aim += count)
      }
    }
    horizontal * depth
  }

}
