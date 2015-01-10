class J10Bot {
  def respond(input: String): String = {
    import J10Bot._
    val t = time.findFirstIn(input).map {
      case time(n) => n
    }.getOrElse(0).toString.toInt

    val v = view.findFirstIn(input).get
    val boardSize = Math.sqrt(v.size).toInt

    val c: String = cmd.findFirstIn(input).get
    c match {
      case "React" =>
        val dx = rnd.nextInt(3) - 1
        val dy = rnd.nextInt(3) - 1
        "Move(direction=" + dx + ":" + dy + ")|Log(text=" + v + ")"
      case _ =>
        "Status(text=J10)"
    }
  }
}
object J10Bot {
  val time = """time=(\d+)""".r
  val cmd = """([^(]*)""".r
  val view = """view=([^,]*)""".r

  import util.Random

  val rnd = new Random()

  case class View(cells: String) {
    val size = math.sqrt(cells.length).toInt

    def apply(index: Int) = cells.charAt(index)

    def offsetToNearest(c: Char) = {
      val relativePositions =
        cells
          .view
          .zipWithIndex
          .filter(_._1 == c)
          .map(p => relPosFromIndex(p._2))
      if (relativePositions.isEmpty)
        None
      else
        Some(relativePositions.minBy(_.length))
    }
  }
}

class ControlFunctionFactory {
  def create = new J10Bot().respond _
}
