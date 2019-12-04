object Day04 {

  def main(args: Array[String]): Unit = {
    val range = 136818 to 685979
    val count = range.count(meetEasyCriterias)
    println(s"Got $count with correct criterias")

    val count2 = range.count(meetHardCriterias)
    println(s"Got $count2 with harder criterias")
  }

  def meetEasyCriterias(n: Int): Boolean = {
    val seq = n.toString.toSeq
    hasSixDigits(seq) && hasIncreasingDigits(seq) && hasTwoSameAdjacentDigits(seq)
  }

  def meetHardCriterias(n: Int): Boolean = {
    val seq = n.toString.toSeq
    hasSixDigits(seq) && hasIncreasingDigits(seq) && hasTwoSameAdjacentDigits(seq) && hasNotPartOfLargerGroup(seq)
  }

  @inline def hasSixDigits(n: Seq[Char]): Boolean = {
    n.length == 6
  }

  def hasTwoSameAdjacentDigits(n: Seq[Char]): Boolean = {
    n.sliding(2).exists(group => group.head == group(1))
  }

  def hasIncreasingDigits(n: Seq[Char]): Boolean = {
    n.sliding(2).forall(group => group.head.toInt <= group(1).toInt)
  }

  def hasNotPartOfLargerGroup(n: Seq[Char]): Boolean = {
    val doubles = n.sliding(2).filter(group => group.head == group(1)).toList
    doubles.groupBy(identity).exists { case (_, groups) => groups.toSet.size == groups.size }
  }
}
