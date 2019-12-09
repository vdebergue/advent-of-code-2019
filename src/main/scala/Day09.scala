object Day09 {

  def main(args: Array[String]): Unit = {
    val file = scala.io.Source.fromURL(getClass.getResource("Day09.txt"))
    val inputStr = file.mkString

    val program = Day02.parseProgram(inputStr).map(_.toLong)
    val outputs = runProgram(program, Seq(1))
    println(s"outputs = $outputs")

    val outputs2 = runProgram(program, Seq(2))
    println(s"outputs2 = $outputs2")
  }

  def runProgram(program: Seq[Long], inputs: Seq[Long] = Seq.empty): Seq[Long] = {
    // grow memory to handle programs
    val memory = program.toArray.padTo(math.max(2 * program.size, 1000), 0L)
    val state = MemoryState(memory, input =  inputs.toList)
    evaluateProgram(state)
    state.output
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
    val memory = line.split(",").map(_.toLong)
    MemoryState(memory, pointer = 0)
  }

  def parseInstruction(n: Long): Instruction = {
    val format = new java.text.DecimalFormat("00000")
    val str = format.format(n)
    Instruction(
      str.slice(3, 5).toInt,
      Seq(ParameterMode.parse(str(2)), ParameterMode.parse(str(1)), ParameterMode.parse(str(0)))
    )
  }

  case class MemoryState(
                          memory: Array[Long],
                          var pointer: Int = 0,
                          var input: List[Long] = Nil,
                          var output: Vector[Long] = Vector.empty,
                          var finished: Boolean = false,
                          var relativeBase: Int = 0
                        )

  sealed trait ParameterMode
  object ParameterMode {
    case object Position extends ParameterMode
    case object Immediate extends ParameterMode
    case object Relative extends ParameterMode

    def parse(char: Char): ParameterMode = char match {
      case '0' => Position
      case '1' => Immediate
      case '2' => Relative
    }
  }
  case class Instruction(opCode: Int, modes: Seq[ParameterMode])

  sealed trait Operation {
    def numParameters: Int
    def execute(instruction: Instruction)(implicit state: MemoryState): Unit

    def getParameter(argNumber: Int, instruction: Instruction)(implicit state: MemoryState): Long = {
      val mode = instruction.modes(argNumber)
      mode match {
        case ParameterMode.Immediate => state.memory(state.pointer + argNumber + 1)
        case _  => state.memory(getAddress(argNumber, instruction))
      }
    }

    def getAddress(argNumber: Int, instruction: Instruction)(implicit state: MemoryState): Int = {
      val mode = instruction.modes(argNumber)
      val argPosition = state.pointer + argNumber + 1
      mode match {
        case ParameterMode.Position => state.memory(argPosition).toInt
        case ParameterMode.Immediate => sys.error("Illegal")
        case ParameterMode.Relative => (state.relativeBase + state.memory(argPosition)).toInt
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
      case 9 => AdjustRelativeBase
      case 99 => Stop
    }

    case object Add extends Operation {
      val numParameters = 3
      override def execute(instruction: Instruction)(implicit state: MemoryState): Unit = {
        val res = getParameter(0, instruction) + getParameter(1, instruction)
        val outPost = getAddress(2, instruction)
        state.memory.update(outPost, res)
      }
    }
    case object Mult extends Operation {
      val numParameters = 3
      def execute(instruction: Instruction)(implicit state: MemoryState): Unit = {
        val res = getParameter(0, instruction) * getParameter(1, instruction)
        val outPost = getAddress(2, instruction)
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
        val outPost = getAddress(0, instruction)
        state.memory.update(outPost, state.input.head)
        state.input = state.input.tail
      }
    }

    case object SetOutput extends Operation {
      val numParameters = 1
      def execute(instruction: Instruction)(implicit state: MemoryState): Unit = {
        val param = getParameter(0, instruction)
        state.output = state.output :+ param
      }
    }

    case object JumpIfTrue extends Operation {
      val numParameters = 2
      def execute(instruction: Instruction)(implicit state: MemoryState): Unit = {
        val pIf = getParameter(0, instruction)
        if (pIf != 0) {
          state.pointer = getParameter(1, instruction).toInt
        }
      }
    }

    case object JumpIfFalse extends Operation {
      val numParameters = 2
      def execute(instruction: Instruction)(implicit state: MemoryState): Unit = {
        val pIf = getParameter(0, instruction)
        if (pIf == 0) {
          state.pointer = getParameter(1, instruction).toInt
        }
      }
    }

    case object LessThan extends Operation {
      val numParameters = 3
      def execute(instruction: Instruction)(implicit state: MemoryState): Unit = {
        val p1 = getParameter(0, instruction)
        val p2 = getParameter(1, instruction)
        val res = if (p1 < p2) 1 else 0
        val outPost = getAddress(2, instruction)
        state.memory.update(outPost, res)
      }
    }

    case object Equals extends Operation {
      val numParameters = 3
      def execute(instruction: Instruction)(implicit state: MemoryState): Unit = {
        val p1 = getParameter(0, instruction)
        val p2 = getParameter(1, instruction)
        val res = if (p1 == p2) 1 else 0
        val outPost = getAddress(2, instruction)
        state.memory.update(outPost, res)
      }
    }

    case object AdjustRelativeBase extends Operation {
      val numParameters = 1
      def execute(instruction: Instruction)(implicit state: MemoryState): Unit = {
        val p = getParameter(0, instruction)
        state.relativeBase = (state.relativeBase + p).toInt
      }
    }
  }
}
