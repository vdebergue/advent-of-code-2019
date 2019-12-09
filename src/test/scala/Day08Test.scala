import org.scalatest.Matchers._
import org.scalatest._

class Day08Test extends FunSuite {

  test("toLayers") {
    Day08.toLayers(Seq(1,2,3,4,5,6,7,8,9,0,1,2), 3, 2) should equal (Seq(
      Seq(Seq(1, 2, 3), Seq(4,5,6)),
      Seq(Seq(7,8,9), Seq(0,1,2))
    ))
  }

  test("stackLayers") {
    val layers = Day08.toLayers(Seq(0,2,2,2,1,1,2,2,2,2,1,2,0,0,0,0), 2, 2)
    Day08.stackLayers(layers) should equal (Seq(Seq(0,1), Seq(1, 0)))
  }
}