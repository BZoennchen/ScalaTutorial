package hackerrank

object StringMingling {
  def main(args: Array[String]) {
    val lines = io.Source.stdin.getLines().toList
    //val lines = List("abcd", "efgh")

    println(lines(0).zip(lines(1)).map(pair => pair._1 + "" + pair._2).mkString)

  }
}
