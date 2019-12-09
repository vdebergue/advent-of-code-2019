import org.scalatest.Matchers._
import org.scalatest._

class Day09Test extends FunSuite {

  test("previous behaviour") {
    val pgrm = "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
    val memory = Day02.parseProgram(pgrm).toList.map(_.toLong)

    val state1 = Day09.MemoryState(memory.toArray, input = List(3), pointer = 0)
    Day09.evaluateProgram(state1)
    state1.output should be (Seq(999))
  }

  test("evaluate") {
    val program = Seq(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99).map(_.toLong)
    Day09.runProgram(program) should equal (program)
  }
}