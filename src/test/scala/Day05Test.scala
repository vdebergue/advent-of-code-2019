import org.scalatest.Matchers._
import org.scalatest._

class Day05Test extends FunSuite {

  test("should run programs") {
    Day05.runProgram(Seq(1,0,0,0,99), 1) should === (2)
    Day05.runProgram(Seq(2,3,0,3,99), 1) should === (2)
    Day05.runProgram(Seq(2,4,4,5,99,0), 1) should === (2)
    Day05.runProgram(Seq(1,1,1,4,99,5,6,0,99), 1) should === (30)
  }
}