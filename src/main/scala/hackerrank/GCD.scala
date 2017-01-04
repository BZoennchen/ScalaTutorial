package hackerrank

object GCD {
  def main(args: Array[String]): Unit = {
    println(gcd(List(7,21,35)))
  }

  def gcd(list: Seq[Int]): Int = list match {
    case Nil => throw new IllegalArgumentException("empty list")
    case x :: y :: Nil => gcd(x, y)
    case x :: tail => gcd(gcd(x, tail(0)) :: tail.drop(1))
  }

  def gcd(a: Int, b: Int): Int = {
    if(a == b) a
    else if(a > b) gcd(a - b, b)
    else gcd(a, b - a)
  }
}
