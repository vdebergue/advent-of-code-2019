import org.scalatest.Matchers._
import org.scalatest._

class Day02Test extends FunSuite {

  test("should run programs") {
    Day02.evaluateProgram(Array(1,0,0,0,99)) should === (Array(2,0,0,0,99))
    Day02.evaluateProgram(Array(2,3,0,3,99)) should === (Array(2,3,0,6,99))
    Day02.evaluateProgram(Array(2,4,4,5,99,0)) should === (Array(2,4,4,5,99,9801))
    Day02.evaluateProgram(Array(1,1,1,4,99,5,6,0,99)) should === (Array(30,1,1,4,2,5,6,0,99))
  }


}