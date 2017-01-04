package hackerrank

object Pascal {
  def main(args: Array[String]): Unit = {
    val N = readLine().trim().toInt
    for(n <- 0 to N-1) {
      for(k <- 0 to n) {
        print(pascal(n, k) + " ")
      }
      println()
    }
  }

  def pascal(n: Int, k: Int): Int = (n, k) match {
    case (_, 0) => 1
    case (_, 1) => n
    case (n, k) if n == k => 1
    case (n, k) if n < k => throw new IllegalArgumentException(n + " > " + k + " is illegal.")
    case (_, _) => pascal(n-1, k) + pascal(n-1, k-1)
  }
}
