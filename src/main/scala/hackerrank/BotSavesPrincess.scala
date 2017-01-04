package hackerrank

object BotSavesPrincess {
  def main(args: Array[String]) = {
    val m = Console.readLine.toInt
    val grid = new Array[Array[Char]](m)
    for (i <- 0 until m) {
      grid.update(i, Console.readLine().toCharArray)
    }
    displayPathtoPrincess(m,grid)
  }

  /* Refer to Output format section for more details */
  def displayPathtoPrincess(m:Int,grid:Array[Array[Char]])={

    var m, p = (0,0)


    for(y <- 0 to grid.length-1; x <- 0 to grid(0).length-1) {
      if(grid(y)(x) == 'p') {
        p = (x, y)
      }
      else if(grid(x)(y) == 'm') {
        m = (x, y)
      }
    }

    val dx = p._1-m._1
    val dy = p._2-m._2

    if(dx > 0) {
      (1 to dx).map(i => "RIGHT").foreach(println(_))
    }
    else if(dx < 0) {
      (1 to (-dx)).map(i => "LEFT").foreach(println(_))
    }

    if(dy > 0) {
      (1 to dy).map(i => "DOWN").foreach(println(_))
    }
    else if(dy < 0) {
      (1 to (-dy)).map(i => "UP").foreach(println(_))
    }
  }
}
