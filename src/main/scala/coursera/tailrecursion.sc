import scala.annotation.tailrec

def fac(n: Int) : Int = {
  n match {
    case 0 => 1
    case 1 => 1
    case _ => n * fac(n - 1)
  }
}

def fac2(n: Int) : Int = {
  @tailrec
  def loop(n: Int, acc: Int) : Int = {
    n match {
      case 0 => acc
      case 1 => acc
      case _ => loop(n-1, acc * n)
    }
  }

  loop(n, 1)
}

fac(6)
fac2(6)