import Day10.Point
import org.scalatest.Matchers._
import org.scalatest._

class Day10Test extends FunSuite {

  test("countVisibleAsteroids") {
    val input =
      """|.#..#
        |.....
        |#####
        |....#
        |...##""".stripMargin
    val points = Day10.parseInput(input)

    Day10.countVisibleAsteroids(Day10.Point(3,4), points) should equal (8)
    Day10.countVisibleAsteroids(Day10.Point(1,0), points) should equal (7)
    Day10.countVisibleAsteroids(Day10.Point(4,4), points) should equal (7)
    Day10.countVisibleAsteroids(Day10.Point(0,2), points) should equal (6)
    Day10.countVisibleAsteroids(Day10.Point(4,2), points) should equal (5)
    println("------")
    Day10.countVisibleAsteroids(Day10.Point(2,2), points) should equal (7)
  }

  test("findBestLocation") {
    val input =
      """|.#..#
         |.....
         |#####
         |....#
         |...##""".stripMargin
    val points = Day10.parseInput(input)
    Day10.findBestLocation(points) should equal (Day10.Point(3,4) -> 8)
  }

  test("zap") {
    val input=""".#..##.###...#######
                |##.############..##.
                |.#.######.########.#
                |.###.#######.####.#.
                |#####.##.#.##.###.##
                |..#####..#.#########
                |####################
                |#.####....###.#.#.##
                |##.#################
                |#####.##.###..####..
                |..######..##.#######
                |####.##.####...##..#
                |.#####..#.######.###
                |##...#.##########...
                |#.##########.#######
                |.####.#.###.###.#.##
                |....##.##.###..#####
                |.#.#.###########.###
                |#.#.#.#####.####.###
                |###.##.####.##.#..##""".stripMargin

    val points = Day10.parseInput(input)
    val (best, _) = Day10.findBestLocation(points)
    best should equal (Day10.Point(11, 13))

    val zapped = Day10.laserUpAsteroids(best, points)
    zapped.head should equal (Point(11,12))
    zapped(1) should equal (Point(12,1))
    zapped(2) should equal (Point(12,2))
    zapped(9) should equal (Point(12,8))

  }
}