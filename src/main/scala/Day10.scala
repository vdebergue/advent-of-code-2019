object Day10 {

  def main(args: Array[String]): Unit = {
    val file = scala.io.Source.fromURL(getClass.getResource("Day10.txt"))
    val inputStr = file.mkString

    val asteroids = parseInput(inputStr)
    val (best, count) = findBestLocation(asteroids)
    println(s"Best loc is $best with $count visible asteroids")

    val dust = laserUpAsteroids(best, asteroids)
    println(s"200 = ${dust(199)}")
  }

  def findBestLocation(asteroids: Seq[Point]): (Point, Int) = {
    asteroids
      .map(origin => origin -> countVisibleAsteroids(origin, asteroids))
      .maxBy(_._2)
  }

  def laserUpAsteroids(origin: Point, asteroids: Seq[Point]): Seq[Point] = {
    val byAngle = asteroidsByAngleAndDistance(origin, asteroids)
      .map { case (key, seq) => key -> seq.to(collection.mutable.Queue) }
      .to(collection.mutable.Map)
    val angles = byAngle.keys.toList.sorted
    var currentAngleIndex = angles.indexWhere(_ == -math.Pi / 2)
    println(s"# Angles = ${angles.size} - start at index $currentAngleIndex => ${angles(currentAngleIndex)}")
    println(s"origin= $origin => ${byAngle.get(angles(currentAngleIndex))}")
    val output = collection.mutable.ArrayBuffer.empty[Point]

    while (byAngle.nonEmpty) {
      val angle = angles(currentAngleIndex)
      val inLine = byAngle.getOrElse(angle, collection.mutable.Queue.empty)
      if (inLine.nonEmpty) {
        val asteroid = inLine.removeHead()
        output.append(asteroid)
        if (inLine.isEmpty) {
          byAngle.remove(angle)
        }
      }
      currentAngleIndex = (currentAngleIndex + 1) % angles.length
    }
    output.toSeq
  }

  def countVisibleAsteroids(origin: Point, asteroids: Seq[Point]): Int = {
    val byAngle = asteroidsByAngleAndDistance(origin, asteroids.filter(_ != origin))
    byAngle.keys.size
  }

  type SkyMap = Seq[Seq[Cell]]
  case class Point(x: Int, y: Int) {
    def slope(origin: Point): Double = {
      (this.y - origin.y).toDouble / (this.x - origin.x).toDouble
    }

    def distance(origin: Point): Double = {
      math.sqrt(math.pow((this.y - origin.y).toDouble, 2) + math.pow((this.x - origin.x).toDouble, 2))
    }

    def angle(origin: Point): Double = math.atan2((this.y - origin.y).toDouble, (this.x - origin.x).toDouble)
  }

  sealed trait Cell
  object Cell {
    case object Asteroid extends Cell
    case object Empty extends Cell
  }

  def parseInput(str: String): Seq[Point] = {
    val map = str
      .split("\n")
      .map { line =>
        line.map {
          case '#' => Cell.Asteroid
          case '.' => Cell.Empty
        }
      }
    asteroidPoints(map)
  }

  def asteroidPoints(map: SkyMap): Seq[Point] = {
    map.zipWithIndex.flatMap {
      case (row, y) =>
        row.zipWithIndex.flatMap {
          case (cell, x) =>
            if (cell == Cell.Asteroid) Seq(Point(x, y)) else Seq.empty
        }
    }
  }

  def asteroidsByAngleAndDistance(origin: Point, asteroids: Seq[Point]): Map[Double, Seq[Point]] = {
    asteroids
      .groupBy(asteroid => asteroid.angle(origin))
      .map { case (angle, values) => angle -> values.sortBy(_.distance(origin)) }
  }
}
