package hackerrank

/**
  * Created by bzoennchen on 14.08.16.
  */
object PolygonArea {
  type Matrix2D = ((Int, Int), (Int, Int));

  def main(args: Array[String]) {
    def sub(p1: (Int, Int), p2: (Int, Int)): (Int, Int) = {
      (p1._1 - p2._1, p1._2 - p2._2)
    }
    def distance(p1: (Int, Int), p2: (Int, Int)): Double = {
      val vec = sub(p1, p2)
      Math.sqrt(vec._1 * vec._1 + vec._2 * vec._2)
    }
    def shiftRight[A](seq : Seq[A]): Seq[A] = {
      seq(seq.length-1) +: seq.take(seq.length-1)
    }

    def shiftLeft[A](seq : Seq[A]): Seq[A] = {
      seq.drop(1) :+ seq(0)
    }

    def det(matrix: Matrix2D): Double = {
      matrix._1._1 * matrix._2._2 - matrix._2._1 * matrix._1._2;
    }

    val lines = io.Source.stdin.getLines().toList
    //val lines = List("3","1 2", "1 5", "3 5", "3 2")
    var N = lines(0).toInt
    val pairs = lines.drop(1).map(line => {
      val arr = line.split(" ")
      (arr(0).toInt, arr(1).toInt)
    })

    println(pairs.zip(shiftLeft(pairs)).map(det(_)).sum * 0.5)
  }
}
