package hackerrank

// Not functional!
object Crosswords {
  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines().toList

    /*val lines = List(
      "+-++++++++",
      "+-++++++++",
      "+-++++++++",
      "+-----++++",
      "+-+++-++++",
      "+-+++-++++",
      "+++++-++++",
      "++------++",
      "+++++-++++",
      "+++++-++++",
      "LONDON;DELHI;ICELAND;ANKARA")*/

    val grid = lines.take(lines.length-1)
    val words = lines(lines.length-1).split(";").toList

    val coords = (for(y <- 0 to grid.length; x <- 0 to grid(0).length) yield (x,y)).filter(coord => isEntryPoint(grid, coord._1, coord._2)).toSet
    insertAll(grid, words, coords).foreach(println(_))
  }

  def insertAll(grid: List[String], words: List[String], coords: Set[(Int, Int)]): List[String] = {
    insertAll2(grid, words, coords)._1
  }

  def insertAll2(grid: List[String], words: List[String], coords: Set[(Int, Int)]): (List[String], Boolean) = {
    //grid.foreach(println(_))
    //println()
    if(words.isEmpty){
      return (grid, true)
    }
    else {
      val word = words(0)
      val newWords = words.drop(1)
      for(coord <- coords) {
        if(canInsertedRight(grid, coord._1, coord._2, word)) {
          val pair = insertAll2(insertedRight(grid, coord._1, coord._2, word), newWords, coords - coord)
          if(pair._2) {
            return pair
          }
        }
      }

      for(coord <- coords) {
        if(canInsertedBot(grid, coord._1, coord._2, word)) {
          val pair = insertAll2(insertedBot(grid, coord._1, coord._2, word), newWords, coords - coord)
          if(pair._2) {
            return pair
          }
        }
      }
    }

    return (grid, false)
  }

  def canInsertedRight(grid: List[String], x: Int, y: Int, word: String): Boolean = {
    if((isRightEntryPoint(grid, x, y) || (isSingleEntryPoint(grid, x, y) && word.length == 1)) && isInsideGrid(grid, x + word.length-1, y)) {
      val line = grid(y)
      for(i <- x to x + word.length - 1) {
        if(!isInsideGrid(grid, i, y) || (line.charAt(i) != '-' && line.charAt(i) != word.charAt(i-x))) {
          return false
        }
      }
      return isOutsideGrid(grid, x + word.length, y) || isObstaclePosition(grid, x + word.length, y)
    }
    return false
  }

  def canInsertedBot(grid: List[String], x: Int, y: Int, word: String): Boolean = {
    if(isBotEntryPoint(grid, x, y) && isInsideGrid(grid, x, y + word.length-1)) {
      val line = (0 to grid.length-1).map(i => grid(i).charAt(x).toString).reduce((s1, s2) => s1 + s2)
      for(i <- y to y + word.length - 1) {
        if(!isInsideGrid(grid, x, i) || (line.charAt(i) != '-' && line.charAt(i) != word.charAt(i-y))) {
          return false
        }
      }
      return isOutsideGrid(grid, x, y + word.length) || isObstaclePosition(grid, x, y + word.length)
    }
    return false
  }

  def insertedRight(grid: List[String], x: Int, y: Int, word: String): List[String] = {
      val line = grid(y)
      val newLine = line.substring(0, x) + word + (if(line.length > x+word.length) {line.substring(x+word.length)} else {""})
      val newGrid = grid.zipWithIndex.map(pair => (if(pair._2 != y) pair._1 else newLine))
      return newGrid
  }

  def insertedBot(grid: List[String], x: Int, y: Int, word: String): List[String] = {
    return grid.zipWithIndex.map(pair => {
      if(pair._2 < y || pair._2 > y + word.length-1) {
        pair._1
      } else {
        pair._1.substring(0, x) + word.charAt(pair._2-y) + (if(pair._1.length > x + 1) pair._1.substring(x+1) else "")
      }
    })
  }

  def isEntryPoint(grid: List[String], x: Int, y: Int): Boolean = {
    isBotEntryPoint(grid, x, y) || isRightEntryPoint(grid, x, y) || isSingleEntryPoint(grid, x, y)
  }

  def isInsideGrid(grid: List[String], x: Int, y: Int): Boolean = {
    y >= 0 && grid.length > y && x >= 0 && grid(0).length > x
  }

  def isBotEntryPoint(grid: List[String], x: Int, y: Int): Boolean = {
    return  isInsideGrid(grid, x, y) &&
            isInsideGrid(grid, x , y + 1) &&
            isCharPosition(grid, x, y) &&
            isCharPosition(grid, x, y + 1) &&
            (isOutsideGrid(grid, x, y - 1) || isObstaclePosition(grid, x, y -1));
  }

  def isRightEntryPoint(grid: List[String], x: Int, y: Int): Boolean = {
    return  isInsideGrid(grid, x, y) &&
            isInsideGrid(grid, x + 1, y) &&
            isCharPosition(grid, x, y) &&
            isCharPosition(grid, x + 1, y) &&
            (isOutsideGrid(grid, x - 1, y) || isObstaclePosition(grid, x - 1, y));
  }

  def isSingleEntryPoint(grid: List[String], x: Int, y: Int): Boolean = {
    return  isInsideGrid(grid, x, y) &&
            isFreePosition(grid, x, y) &&
            (isOutsideGrid(grid, x + 1, y) || isObstaclePosition(grid, x + 1, y)) &&
            (isOutsideGrid(grid, x - 1, y) || isObstaclePosition(grid, x - 1, y)) &&
            (isOutsideGrid(grid, x, y + 1) || isObstaclePosition(grid, x, y + 1)) &&
            (isOutsideGrid(grid, x, y - 1) || isObstaclePosition(grid, x, y - 1))
  }

  def isOutsideGrid(grid: List[String], x: Int, y: Int): Boolean = {
    !isInsideGrid(grid, x, y)
  }

  def isCharPosition(grid: List[String], x: Int, y: Int): Boolean = {
    isFreePosition(grid, x, y) || !isObstaclePosition(grid, x, y)
  }

  def isFreePosition(grid: List[String], x: Int, y: Int): Boolean = {
    grid(y).charAt(x) == '-';
  }

  def isObstaclePosition(grid: List[String], x: Int, y: Int): Boolean = {
    grid(y).charAt(x) == '+'
  }
}
