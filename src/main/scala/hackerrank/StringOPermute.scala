package hackerrank

object StringOPermute {
  def main(args: Array[String]) {
    val lines = io.Source.stdin.getLines().toList
    //val lines = List("2", "abcd", "efgh")
    val cases = lines(0).toInt
    val words = lines.drop(1)

    val even = words.map(word => word.indices.filter(i => i % 2 == 0).map(i => word.charAt(i)))

    val odd = words.map(word => word.indices.filter(i => i % 2 != 0).map(i => word.charAt(i)))


    odd.indices.foreach(i => {
      odd(i).zip(even(i)).foreach(pair => {
        print(pair._1)
        print(pair._2)
      })
      println()
    })
  }
}
