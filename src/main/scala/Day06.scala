import scala.annotation.tailrec

object Day06 {

  def main(args: Array[String]): Unit = {
    val file = scala.io.Source.fromURL(getClass.getResource("Day06.txt"))
    val inputStr = file.mkString
    val count = countDirectAndIndirectOrbits(inputStr)
    println(s"Got count=$count")

    val jumps = countJumps(inputStr, "YOU", "SAN")
    println(s"Jumps = $jumps")
  }

  def countJumps(input: String, pointA: String, pointB: String): Int = {
    val orbits = parseInput(input)
    val graph = Graph(orbits)
    val aPath = graph.getDirectAndIndirectOrbits(pointA)
    val bPath = graph.getDirectAndIndirectOrbits(pointB)
    val meetingPointOpt = aPath.find(orb => bPath.exists(_.center == orb.center)).map(_.center)
    println(s"Found meeting point = $meetingPointOpt")
    val meetingPoint = meetingPointOpt.get
    // Path is not absolutly correct, we don't have the 2 orbits with the meeting points in them
    val totalPath = aPath.takeWhile(_.center != meetingPoint) ++ bPath.takeWhile(_.center != meetingPoint)
    println(s"path is $totalPath")
    // Path has 2 more orbits (you and santa) but also two missing, so result is correct like it is
    totalPath.size
  }

  def countDirectAndIndirectOrbits(input: String): Int = {
    val orbits = parseInput(input)
    val graph = Graph(orbits)
    val objects = graph.listObjects().toList
    objects.map(obj => graph.getDirectAndIndirectOrbits(obj).size).sum
  }

  def parseInput(str: String): Seq[Orbit] = {
    str.split("\n").toList.map { orb =>
      val Array(center, around) = orb.split(')')
      Orbit(center, around)
    }
  }

  // Edges of the graph
  case class Orbit(center: String, around: String)
  case class Graph(edges: Seq[Orbit], center: String = "COM") {

    def getOrbitsAround(obj: String): Seq[Orbit] = {
      edges.filter(_.center == obj)
    }
    def getCenterOf(obj: String): Orbit = {
      edges.filter(_.around == obj).head
    }
    def listObjects(): Set[String] = {
      edges.flatMap(e => Seq(e.center, e.around)).toSet
    }

    // @tailrec
    final def getDirectAndIndirectOrbits(obj: String): Seq[Orbit] = {
      if (obj == center) Seq.empty
      else {
        val edge = getCenterOf(obj)
        edge +: getDirectAndIndirectOrbits(edge.center)
      }
    }
  }
}
