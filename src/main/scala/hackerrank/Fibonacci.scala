package hackerrank

object Fibonacci {
  def main(args: Array[String]): Unit = {
    println(fib(5))
  }

  def fib(n: Int): Int = n match {
    case 0 => 0
    case 1 => 0
    case 2 => 1
    case _ => fib(n - 1) + fib(n - 2)
  }
}
