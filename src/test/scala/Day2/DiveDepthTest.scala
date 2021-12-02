package Day2
import org.scalatest.funsuite.AnyFunSuite
import scala.io.Source

class DiveDepthTest extends AnyFunSuite {
  test("down") {
    assert(DiveDepth.calculateDepth(Array("forward 3", "down 5")) == 15)
  }

  test("up") {
    assert(DiveDepth.calculateDepth(Array("forward 5", "up 7")) == -35)
  }

  test("up and down") {
    assert(DiveDepth.calculateDepth(Array("forward 5", "up 3", "down 7")) == 20)
  }

  test("example calculation should equal 150") {
    assert(DiveDepth.calculateDepth(Array("forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2")) == 150)
  }

  test("calculate depth given input file") {
    assert(DiveDepth.calculateDepth(Source.fromFile("Day2" ).getLines().toArray) == 1507611)
  }

  test("depth remains 0 without aim") {
    assert(DiveDepth.calculateDepthWithAim(Array("forward 3")) == 0)
  }

  test("aim exists and expected calculation result") {
    assert(DiveDepth.calculateDepthWithAim(Array("forward 4", "down 2", "forward 6")) == 120)
  }

  test("gets expected dive depth with aim of given example") {
    assert(DiveDepth.calculateDepthWithAim(Array("forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2")) == 900)
  }

  test("calculate depth with aim given input file") {
    assert(DiveDepth.calculateDepthWithAim(Source.fromFile("Day2" ).getLines().toArray) == 1880593125)
  }

}
