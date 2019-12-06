import org.scalatest.Matchers._
import org.scalatest._

class Day06Test extends FunSuite {

  test("should count orbits") {
    val input = """COM)B
                  |B)C
                  |C)D
                  |D)E
                  |E)F
                  |B)G
                  |G)H
                  |D)I
                  |E)J
                  |J)K
                  |K)L""".stripMargin

    Day06.countDirectAndIndirectOrbits(input) should equal (42)
  }

  test("Should count jumps") {
    val input =
      """|COM)B
        |B)C
        |C)D
        |D)E
        |E)F
        |B)G
        |G)H
        |D)I
        |E)J
        |J)K
        |K)L
        |K)YOU
        |I)SAN""".stripMargin

    Day06.countJumps(input, "YOU", "SAN") should equal (4)
  }


}