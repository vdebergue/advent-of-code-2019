import Day05.{parseInstruction, MemoryState}

object Day07 {

  def main(args: Array[String]): Unit = {
    val file = scala.io.Source.fromURL(getClass.getResource("Day07.txt"))
    val inputStr = file.mkString
    val program = Day02.parseProgram(inputStr).toList

    val best = findBestOutput(5, program)
    println(s"Best sequence output is $best")

    val feedback = findBestFeedbackLoop(program)
    println(s"f = $feedback")
  }

  def findBestFeedbackLoop(program: Seq[Int]): Int = {
    val ampls = 5 to 9
    val permutations = ampls.permutations
    permutations.map(s => feedbackLoop(s, program)).max
  }

  def findBestOutput(numAmpli: Int, program: Seq[Int]): Int = {
    val ampls = 0 until numAmpli
    val permutations = ampls.permutations
    permutations.map(settings => runSettings(settings, program)).max
  }

  def runSettings(settings: Seq[Int], program: Seq[Int]): Int = {
    val output = settings.foldLeft(0) { case (input, phase) => runAmplifier(phase, input, program) }
    println(s"setting $settings -> $output")
    output
  }

  def runAmplifier(phaseSetting: Int, inputSignal: Int, program: Seq[Int]): Int = {
    val prog = program.toArray
    val state = MemoryState(prog, pointer = 0, input = List(phaseSetting, inputSignal))
    val finalState = Day05.evaluateProgram(state)
    finalState.output.get
  }

  def feedbackLoop(settings: Seq[Int], program: Seq[Int]): Int = {
    val states = settings.map(s => MemoryState(program.toArray, input = List(s), pointer = 0))
    var signal = 0
    var amplifierIndex = 0
    var finished = false
    while (!finished) {
      val amplifier = states(amplifierIndex)
      amplifier.input = amplifier.input :+ signal
      var amplifierFinished = false

      while (!amplifierFinished) {
        val instruction = Day05.parseInstruction(amplifier.memory(amplifier.pointer))
        val operation = Day05.Operation.toOperation(instruction.opCode)
        val previousPointer = amplifier.pointer
        operation.execute(instruction)(amplifier)
        if (operation == Day05.Operation.SetOutput) {
          signal = amplifier.output.get
          amplifierFinished = true
        }
        if (amplifier.finished) {
          amplifierFinished = true
        }
        if (previousPointer == amplifier.pointer) {
          // advance pointer
          amplifier.pointer = amplifier.pointer + operation.numParameters + 1
        }
      }
      if (amplifierIndex == states.length - 1 && amplifier.finished) {
        finished = true
      }
      amplifierIndex = (amplifierIndex + 1) % states.size
    }
    signal
  }
}
