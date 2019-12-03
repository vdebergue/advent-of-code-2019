import org.scalatest.Matchers._
import org.scalatest._

class Day03Test extends FunSuite {

  test("should run min distance") {
    Day03.runMinDistance("""R8,U5,L5,D3
                |U7,R6,D4,L4""".stripMargin) should equal (6)
    Day03.runMinDistance("""R75,D30,R83,U83,L12,D49,R71,U7,L72
                |U62,R66,U55,R34,D71,R55,D58,R83""".stripMargin) should equal (159)
    Day03.runMinDistance("""R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
                |U98,R91,D20,R16,D67,R40,U7,R15,U6,R7""".stripMargin) should equal (135)
  }

  test("should run min timing") {
    Day03.runMinTiming("""R8,U5,L5,D3
                         |U7,R6,D4,L4""".stripMargin) should equal (30)

    Day03.runMinTiming(
      """R75,D30,R83,U83,L12,D49,R71,U7,L72
        |U62,R66,U55,R34,D71,R55,D58,R83""".stripMargin) should equal (610)

    Day03.runMinTiming(
      """R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
        |U98,R91,D20,R16,D67,R40,U7,R15,U6,R7""".stripMargin) should equal (410)
  }


}