object Day08 {

  def main(args: Array[String]): Unit = {
    val file = scala.io.Source.fromURL(getClass.getResource("Day08.txt"))
    val inputStr = file.mkString

    val input = inputStr.toSeq.map(_.asDigit)
    val layers = toLayers(input, 25, 6)

    val minLayer = layers.minBy { layer =>
      layer.map(r => r.count(_ == 0)).sum
    }
    println(minLayer)
    val num2 = minLayer.map(r => r.count(_ == 2)).sum
    val num1 = minLayer.map(r => r.count(_ == 1)).sum
    println(s"hash = ${num1 * num2}")

    val stacked = stackLayers(layers)
    println(stacked.map(_.mkString(" ")).mkString("\n"))
  }

  def stackLayers(layers: Seq[Seq[Seq[Int]]]): Seq[Seq[Int]] = {
    val height = layers.head.size
    val width = layers.head.head.size
    Seq.tabulate(height, width) {
      case (h, w) =>
        val pixels = layers.map(layer => layer(h)(w))
        pixels.foldLeft(2) {
          case (2, pix)   => pix
          case (other, _) => other
        }
    }
  }

  def toLayers(input: Seq[Int], width: Int, height: Int): Seq[Seq[Seq[Int]]] = {
    val numLayers = input.length / (width * height)
    println(s"numLayers = $numLayers")
    Seq.tabulate(numLayers) { layer =>
      Seq.tabulate(height, width) {
        case (h, w) =>
          val index = layer * (height * width) + h * width + w
          input(index)
      }
    }
  }

}
