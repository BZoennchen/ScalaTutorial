package hackerrank

object RecursiveTrees {
  def main(args: Array[String]) {
    val n = io.Source.stdin.getLines().toList(0).toInt
    val m = 3-(n-2)
    val forest = generateForest(n,m)

    val cols = forest(0).length
    val rows = forest.length

    // fill
    List.fill(63-rows)("_" * 100) ::: forest.map(e => "_" * ((100-cols) / 2) + e + "_" * ((100-cols)/2 + 1)) foreach println
  }

  def generateForest(n: Int, m: Int): List[String] = {
    lazy val space = "_" * (1 << m)
    lazy val cspace = "_" * ((1 << (m+1))-1)
    n match {
      case 1 => generateTree(m)
      case _ => generateTree(m).map(e => (e + cspace) * ((1 << (n-1)) - 1) + e) ::: generateForest(n-1, m+1).map(e => space + e + space)
    }
  }

  def generateTree(n: Int): List[String] = {
    val l = 1 << n
    val space = "_" * l
    stom(l) ::: List.fill(l)(space + "1" + space)
  }

  def stom(n: Int): List[String] = {
    (0 to (n-1)).toList.map(i => ("_" * i) + "1" + ("_" * (2*(n-1-i)+1)) + "1" + ("_" * i) )
  }
}