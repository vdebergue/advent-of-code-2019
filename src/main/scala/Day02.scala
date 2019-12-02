object Day02 {

  def main(args: Array[String]): Unit = {
    val file = scala.io.Source.fromURL(getClass.getResource("Day02.txt"))
    val inputStr = file.mkString
    val program = parseProgram(inputStr).toSeq

    println(s"output: ${runProgram(program, 12, 2)}")

    val target = 19690720
    val result = (0 to 99)
      .flatMap(x => (0 to 99).map(y => (x, y)))
      .find { case (noun, verb) => runProgram(program, noun, verb) == target }

    result match {
      case Some((noun, verb)) => println(s"Found noun=$noun verb=$verb => ${100 * noun + verb}")
      case _                  => println(s"No result found for target $target")
    }
  }

  def runProgram(program: Seq[Int], noun: Int, verb: Int): Int = {
    println(s"Running with noun=$noun and verb=$verb")
    val prog = program.toArray
    // init program
    prog.update(1, noun)
    prog.update(2, verb)
    evaluateProgram(prog)
    prog(0)
  }

  def evaluateProgram(program: Array[Int]): Array[Int] = {
    def add(posA: Int, posB: Int, ret: Int) = {
      val a = program(posA)
      val b = program(posB)
      program.update(ret, a + b)
    }
    def mult(pos1: Int, pos2: Int, ret: Int): Unit = {
      val a = program(pos1)
      val b = program(pos2)
      program.update(ret, a * b)
    }

    var pointer = 0
    var finished = false
    while (!finished) {
      val opcode = program(pointer)
      opcode match {
        case 1  => add(program(pointer + 1), program(pointer + 2), program(pointer + 3))
        case 2  => mult(program(pointer + 1), program(pointer + 2), program(pointer + 3))
        case 99 => finished = true
        case _  => sys.error(s"Unsupported opcode $opcode")
      }
      pointer = pointer + 4
    }
    program
  }

  def parseProgram(line: String): Array[Int] = {
    line.split(",").map(_.toInt)
  }
}
