object Day01 {

  def main(args: Array[String]): Unit = {
    val file = scala.io.Source.fromURL(getClass.getResource("Day01.txt"))
    val lines = file.getLines().toList
    val sum = lines.map { line =>
      getModuleFuel(line.toInt)
    }.sum

    println(s"Total fuel for modules: ${sum}")

    val sumExtended = lines.map { line =>
      getModuleFuelExtended(line.toInt)
    }.sum
    println(s"Adjusted fuel for all modules: $sumExtended")
  }

  def getModuleFuel(module: Int): Int = (Math.floor(module / 3.0) - 2).toInt

  def getModuleFuelExtended(module: Int): Int = {
    val fuel = getModuleFuel(module)
    var additionalFuel = 0
    var fuelForFuel = getModuleFuel(fuel)
    while (fuelForFuel > 0) {
      additionalFuel += fuelForFuel
      fuelForFuel = getModuleFuel(fuelForFuel)
    }
    fuel + additionalFuel
  }
}
