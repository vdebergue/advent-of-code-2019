import org.scalatest.Matchers._
import org.scalatest._

class Day07Test extends FunSuite {

  test("should get max output") {
    val input = Seq(3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0)
    Day07.runSettings(Seq(4,3,2,1,0), input) should equal (43210)
    Day07.findBestOutput(5, input) should equal (43210)
  }

  test("feedbackLoop") {
    println("----------------------")
    val input = Seq(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5)
    Day07.feedbackLoop(Seq(9,8,7,6,5), input) should equal (139629729)
  }
}