package hackerrank

object SierpinskiTriangles {

  val max = 2 << 6

  def main(args: Array[String]) {
    drawTriangles(5)
  }

  def drawTriangles(n: Int) {
    generateTriangles(n,n) foreach println
  }

  def triangle(n: Int): List[String] = {
    lazy val lines = triangle(n - 1)
    n match {
      case 1 => List("1")
      case _ => lines.map(e => ("_" + e + "_")) :+ "1" + lines(lines.length-1)+ "1"
    }
  }

  def generateTriangles(n: Int, m: Int): List[String] = {
    lazy val lastIteration = generateTriangles(n - 1, m)
    lazy val space = "_" * ((max >> m) << (n-2))
    n match {
      case 1 => triangle(max >> m)
      case _ => (lastIteration.map(e => space + e + space)) :::
        (lastIteration.map(e => List.fill(2)(e).mkString("_")))
    }
  }
}
