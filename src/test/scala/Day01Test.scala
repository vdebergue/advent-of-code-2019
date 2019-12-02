import org.scalatest.Matchers._
import org.scalatest._

class Day01Test extends FunSuite {

  test("should calculate correct weights") {
    Day01.getModuleFuel(12) should === (2)
  }

  test("should calculate additional weights") {
    Day01.getModuleFuelExtended(14) should === (2)
    Day01.getModuleFuelExtended(1969) should === (966)
    Day01.getModuleFuelExtended(100756) should === (50346)
  }
}