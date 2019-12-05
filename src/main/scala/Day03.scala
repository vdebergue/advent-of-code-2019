object Day03 {

  case class Movement(direction: Direction, distance: Int)
  sealed trait Direction
  object Direction {
    case object Right extends Direction
    case object Left extends Direction
    case object Up extends Direction
    case object Down extends Direction
  }
  case class Point(x: Int, y: Int) {
    def pointsCrossed(movement: Movement): Seq[Point] = {
      val (xDir, yDir) = movement.direction match {
        case Direction.Left  => -1 -> 0
        case Direction.Right => +1 -> 0
        case Direction.Up    => 0 -> +1
        case Direction.Down  => 0 -> -1
      }
      (1 to movement.distance).map { d =>
        Point(this.x + d * xDir, this.y + d * yDir)
      }
    }

    @inline def distance(other: Point): Int = Math.abs(x - other.x) + Math.abs(y - other.y)
  }
  val CentralPort = Point(0, 0)

  def main(args: Array[String]): Unit = {
    val file = scala.io.Source.fromURL(getClass.getResource("Day03.txt"))
    val input = file.mkString
    val distance = runMinDistance(input)
    println(s"Distance is $distance")

    val timing = runMinTiming(input)
    println(s"Timing is $timing")
  }

  def runMinDistance(input: String) = {
    val cableA :: cableB :: Nil = parseInput(input)
    val intersec = intersections(cableA, cableB)
    val closestIntersection = intersec.minBy(p => p.distance(CentralPort))
    closestIntersection.distance(CentralPort)
  }

  def runMinTiming(input: String): Int = {
    val cableA :: cableB :: Nil = parseInput(input)
    val pointsA = pointsCrossedFromOrigin(cableA)
    val pointsB = pointsCrossedFromOrigin(cableB)
    val intersections = pointsA.toSet.intersect(pointsB.toSet)
    intersections.map { inter =>
      val distA = pointsA.indexOf(inter) + 1
      val distB = pointsB.indexOf(inter) + 1
      distA + distB
    }.min
  }

  def parseInput(input: String): Seq[Seq[Movement]] = {
    input
      .split("\n")
      .map { line =>
        line
          .split(",")
          .map { str =>
            val dir = str.apply(0) match {
              case 'U' => Direction.Up
              case 'D' => Direction.Down
              case 'L' => Direction.Left
              case 'R' => Direction.Right
            }
            val dist = str.drop(1).toInt
            Movement(dir, dist)
          }
          .toList
      }
      .toList
  }

  def pointsCrossedFromOrigin(movements: Seq[Movement]): Seq[Point] = {
    val (_, crossedPoints) = movements.foldLeft((CentralPort, Vector.empty[Point])) {
      case ((point, crossed), mov) =>
        val newPoints = point.pointsCrossed(mov)
        (newPoints.last, crossed ++ newPoints)
    }
    crossedPoints
  }

  def intersections(cableA: Seq[Movement], cableB: Seq[Movement]): Set[Point] = {
    val pointsA = pointsCrossedFromOrigin(cableA)
    val pointsB = pointsCrossedFromOrigin(cableB)
    pointsA.toSet.intersect(pointsB.toSet)
  }
}
