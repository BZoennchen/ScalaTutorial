def from(n: Int): Stream[Int] = n #:: from(n+1)

val nat = from(0)

nat filter (x => x % 2 == 0) take 10 toList

nat filter (x => x % 2 == 1) take 10 toList

