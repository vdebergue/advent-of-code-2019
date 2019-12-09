import org.scalatest.Matchers._
import org.scalatest._

class Day05Test extends FunSuite {

  test("should run programs") {
    Day05.runProgram(Seq(1,0,0,0,99), 1) should === (2)
    Day05.runProgram(Seq(2,3,0,3,99), 1) should === (2)
    Day05.runProgram(Seq(2,4,4,5,99,0), 1) should === (2)
    Day05.runProgram(Seq(1,1,1,4,99,5,6,0,99), 1) should === (30)
  }

  test("test jumps") {
    val pgrm = "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
    val memory = Day02.parseProgram(pgrm).toList

    val state1 = Day05.MemoryState(memory.toArray, input = List(3), pointer = 0)
    Day05.evaluateProgram(state1)
    state1.output should be (Some(999))

    val state2 = Day05.MemoryState(memory.toArray, input = List(8), pointer = 0)
    Day05.evaluateProgram(state2)
    state2.output should be (Some(1000))

    val state3 = Day05.MemoryState(memory.toArray, input = List(10), pointer = 0)
    Day05.evaluateProgram(state3)
    state3.output should be (Some(1001))
  }
}