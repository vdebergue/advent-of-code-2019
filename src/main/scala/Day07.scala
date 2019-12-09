import Day05.MemoryState

object Day07 {

  def main(args: Array[String]): Unit = {
    val file = scala.io.Source.fromURL(getClass.getResource("Day07.txt"))
    val inputStr = file.mkString
    val program = Day02.parseProgram(inputStr).toList

    val best = findBestOutput(5, program)
    println(s"Best sequence output is $best")
  }

  def findBestOutput(numAmpli: Int, program: Seq[Int]): Int = {
    val ampls = 0 until numAmpli
    val permutations = ampls.permutations
    permutations.map(settings => runSettings(settings, program)).max
  }

  def runSettings(settings: Seq[Int], program: Seq[Int]): Int = {
    val output = settings.foldLeft(0){ case (input, phase) => runAmplifier(phase, input, program)}
    println(s"setting $settings -> $output")
    output
  }

  def runAmplifier(phaseSetting: Int, inputSignal: Int, program: Seq[Int]): Int = {
    val prog = program.toArray
    val state = MemoryState(prog, pointer = 0, input = List(phaseSetting, inputSignal))
    val finalState = Day05.evaluateProgram(state)
    finalState.output.get
  }
}
