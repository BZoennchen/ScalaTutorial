package hackerrank

/**
  * TODO: implement quick hull!
  */
object ConvexHull {
  type Point = (Double, Double)
  def Point(x: Double, y: Double) = (x, y)

  class Line(val p1: Point, val p2: Point) {
    override def toString: String = p1.toString() + " - " + p2.toString()
    def slope() : Double = (p2._2 - p1._2) / (p2._1 - p1._1)


    def distance(p: Point): Double = {
      val m = slope()
      val t = p1._2 - m * p1._1
      // 0 = -y + mx + t
      return Math.abs(m * p._1 - p._2 + t)/Math.sqrt(m * m + 1)
    }

    def toTheLeft(p: Point): Boolean = {
      (p._1 - p1._1) * (p2._2 - p1._1) - (p._2 - p1._1) * (p2._1 - p1._1) < 0
    }
  }


  def main(args: Array[String]): Unit = {
    val points = List(Point(8.0,1.0), Point(2.0, 3.0), Point(5.0, 3.0))
    val sortedPoints = points.sortWith((a, b) => a._1.compareTo(b._1) < 0)
    val mostLeft = sortedPoints(0);
    val mostRight = sortedPoints(sortedPoints.length-1)

    println(mostLeft)
    println(mostRight)

    val line = new Line(mostLeft, mostRight)
    val partition = sortedPoints.partition(line.toTheLeft(_))
    findHull(partition._1, mostLeft, mostRight)
    findHull(partition._2, mostRight, mostLeft)

    println(line)
    println(line.distance(3,3))
    println(line.distance(8,1.1))
  }

  def findHull(points: List[Point], p1: Point, p2: Point): Unit = {
    if(points.isEmpty) return
    val line = new Line(p1, p2)
    val farthest = points.maxBy(p => line.distance(p))
  }
}
