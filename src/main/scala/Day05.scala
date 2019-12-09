object Day05 {

  def main(args: Array[String]): Unit = {
    val file = scala.io.Source.fromURL(getClass.getResource("Day05.txt"))
    val inputStr = file.mkString
    val program = Day02.parseProgram(inputStr).toList

    runProgram(program, 1)

    runProgram(program, 5)
  }

  def runProgram(program: Seq[Int], input: Int): Int = {
    val prog = program.toArray
    // init program
    val state = MemoryState(prog, pointer = 0, input = List(input))
    val finalState = evaluateProgram(state)
    finalState.memory(0)
  }

  def evaluateProgram(state: MemoryState): MemoryState = {
    implicit val s = state
    while (!s.finished) {
      val instruction = parseInstruction(state.memory(state.pointer))
      val operation = Operation.toOperation(instruction.opCode)
      val previousPointer = state.pointer
      operation.execute(instruction)
      if (operation == Operation.SetOutput) {
        println(s"ouput == ${state.output}")
      }
      if (previousPointer == state.pointer) {
        // advance pointer
        state.pointer = state.pointer + operation.numParameters + 1
      }
    }
    state
  }

  def parseProgram(line: String): MemoryState = {
    val memory = line.split(",").map(_.toInt)
    MemoryState(memory, pointer = 0)
  }

  def parseInstruction(n: Int): Instruction = {
    val format = new java.text.DecimalFormat("00000")
    val str = format.format(n)
    Instruction(
      str.slice(3, 5).toInt,
      Seq(ParameterMode.parse(str(2)), ParameterMode.parse(str(1)), ParameterMode.parse(str(0)))
    )
  }

  case class MemoryState(
      memory: Array[Int],
      var pointer: Int,
      var input: List[Int] = Nil,
      var output: Option[Int] = None,
      var finished: Boolean = false
  )

  sealed trait ParameterMode
  object ParameterMode {
    case object Position extends ParameterMode
    case object Immediate extends ParameterMode

    def parse(char: Char): ParameterMode = char match {
      case '0' => Position
      case '1' => Immediate
    }
  }
  case class Instruction(opCode: Int, modes: Seq[ParameterMode])

  sealed trait Operation {
    def numParameters: Int
    def execute(instruction: Instruction)(implicit state: MemoryState): Unit

    def getParameter(argNumber: Int, instruction: Instruction)(implicit state: MemoryState): Int = {
      val mode = instruction.modes(argNumber)
      val argPosition = state.pointer + argNumber + 1
      mode match {
        case ParameterMode.Position  => state.memory(state.memory(argPosition))
        case ParameterMode.Immediate => state.memory(argPosition)
      }
    }
  }

  object Operation {
    def toOperation(opCode: Int): Operation = opCode match {
      case 1  => Add
      case 2  => Mult
      case 3  => StoreInput
      case 4  => SetOutput
      case 5  => JumpIfTrue
      case 6  => JumpIfFalse
      case 7  => LessThan
      case 8  => Equals
      case 99 => Stop
    }

    case object Add extends Operation {
      val numParameters = 3
      override def execute(instruction: Instruction)(implicit state: MemoryState): Unit = {
        val res = getParameter(0, instruction) + getParameter(1, instruction)
        val outPost = state.memory(state.pointer + 3)
        state.memory.update(outPost, res)
      }
    }
    case object Mult extends Operation {
      val numParameters = 3
      def execute(instruction: Instruction)(implicit state: MemoryState): Unit = {
        val res = getParameter(0, instruction) * getParameter(1, instruction)
        val outPost = state.memory(state.pointer + 3)
        state.memory.update(outPost, res)
      }
    }

    case object Stop extends Operation {
      val numParameters = 0
      def execute(instruction: Instruction)(implicit state: MemoryState): Unit = {
        state.finished = true
      }
    }

    case object StoreInput extends Operation {
      val numParameters = 1
      def execute(instruction: Instruction)(implicit state: MemoryState): Unit = {
        val outPost = state.memory(state.pointer + 1)
        state.memory.update(outPost, state.input.head)
        state.input = state.input.tail
      }
    }

    case object SetOutput extends Operation {
      val numParameters = 1
      def execute(instruction: Instruction)(implicit state: MemoryState): Unit = {
        val param = getParameter(0, instruction)
        state.output = Some(param)
      }
    }

    case object JumpIfTrue extends Operation {
      val numParameters = 2
      def execute(instruction: Instruction)(implicit state: MemoryState): Unit = {
        val pIf = getParameter(0, instruction)
        if (pIf != 0) {
          state.pointer = getParameter(1, instruction)
        }
      }
    }

    case object JumpIfFalse extends Operation {
      val numParameters = 2
      def execute(instruction: Instruction)(implicit state: MemoryState): Unit = {
        val pIf = getParameter(0, instruction)
        if (pIf == 0) {
          state.pointer = getParameter(1, instruction)
        }
      }
    }

    case object LessThan extends Operation {
      val numParameters = 3
      def execute(instruction: Instruction)(implicit state: MemoryState): Unit = {
        val p1 = getParameter(0, instruction)
        val p2 = getParameter(1, instruction)
        val res = if (p1 < p2) 1 else 0
        val outPost = state.memory(state.pointer + 3)
        state.memory.update(outPost, res)
      }
    }

    case object Equals extends Operation {
      val numParameters = 3
      def execute(instruction: Instruction)(implicit state: MemoryState): Unit = {
        val p1 = getParameter(0, instruction)
        val p2 = getParameter(1, instruction)
        val res = if (p1 == p2) 1 else 0
        val outPost = state.memory(state.pointer + 3)
        state.memory.update(outPost, res)
      }
    }
  }
}
