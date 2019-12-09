import org.scalatest.Matchers._
import org.scalatest._

class Day07Test extends FunSuite {

  test("should get max output") {
    val input = Seq(3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0)
    Day07.runSettings(Seq(4,3,2,1,0), input) should equal (43210)
    Day07.findBestOutput(5, input) should equal (43210)
  }
}