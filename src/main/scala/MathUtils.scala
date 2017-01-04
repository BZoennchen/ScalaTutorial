import NodeState.NodeState

/**
  * @author Benedikt Zoennchen
  */
object MathUtils {

  def add(p1: (Int, Int))(p2: (Int, Int)) = (p1._1 + p2._1, p1._2 + p2._2)

  def distance(p1: (Double, Double), p2: (Double, Double)) = Math.sqrt((p1._1 - p2._1) * (p1._1 - p2._1) + (p1._2 - p2._2) * (p1._2 - p2._2))

  def norm(p: (Double, Double)): (Double, Double) = {
    val norm = Math.sqrt(p._1 * p._1 + p._2 * p._2)
    return (p._1 / norm, p._2 / norm)
  }

  def getMooreNeighbours(point: (Int, Int)): IndexedSeq[(Int, Int)] = {
    val addToPoint = add(point)(_)
    for(i <- -1 to 1; j <- -1 to 1 if (i != 0 || j != 0)) yield addToPoint((i, j))
  }

  def getMooreNeighbours(): IndexedSeq[(Int, Int)] = getMooreNeighbours((0,0))

  def getNeumannNeighborhood(point: (Int, Int), len: Int): IndexedSeq[(Int, Int)] = {
    val addToPoint = add(point)(_)
    for(i <- -len to len; j <- -len to len if (i == 0 && j != 0 || i != 0 && j == 0)) yield addToPoint((i, j))
  }

  def getNeumannNeighborhood(point: (Int, Int)): IndexedSeq[(Int, Int)] = getNeumannNeighborhood((0,0), 1)

  def getNeumannNeighborhood(): IndexedSeq[(Int, Int)]  = getNeumannNeighborhood((0,0))

  def solveQuadratic(a: Double, b: Double, c: Double): (Option[Double], Option[Double]) = {
    if(a != 0) {
      val discr = (b * b) - (4 * a * c)

      if(discr == 0) {
        return (Option.apply(-b / (2.0 * a)), Option.empty[Double])
      }
      else if(discr > 0) {
        return (Option.apply((-b + scala.math.sqrt(discr)) / (2.0 * a)), Option.apply((-b - scala.math.sqrt(discr)) / (2.0 * a)))
      }
    }
    else if(b != 0) {
      return (Option.apply(-c / b), Option.empty[Double])
    }
    return (Option.empty[Double], Option.empty[Double])
  }

  /*def gedouvDif(point: (Int, Int), nodeState: Array[Node][Node]): Double = {

    // x-direction
    nodeState[point._1][point._2] = 1;

  }*/
}