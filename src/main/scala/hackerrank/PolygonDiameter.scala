package hackerrank

/**
  * Created by bzoennchen on 10.08.16.
  */
object PolygonDiameter {

  def main(args: Array[String]) {
    def sub(p1: (Int, Int), p2: (Int, Int)): (Int, Int) = {
      (p1._1 - p2._1, p1._2 - p2._2)
    }
    def distance(p1: (Int, Int), p2: (Int, Int)): Double = {
      val vec = sub(p1, p2)
      Math.sqrt(vec._1 * vec._1 + vec._2 * vec._2)
    }

    //val lines = io.Source.stdin.getLines().toList
    val lines = List("3","1 2", "1 5", "3 5", "3 2")
    var N = lines(0).toInt
    val pairs = lines.drop(1).map(line => {
      val arr = line.split(" ")
      (arr(0).toInt, arr(1).toInt)
    })
    println(pairs.foldLeft((pairs(pairs.length-1), 0.0))((a: ((Int, Int), Double), b: (Int, Int)) => (b, a._2 + distance(a._1,b)))._2)

  }
}