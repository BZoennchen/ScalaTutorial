val integers = new Generator[Int] {
  val rand = new java.util.Random
  override def generate: Int = rand.nextInt()
}

val booleans = new Generator[Boolean] {
  override def generate: Boolean = integers.generate > 0
}

trait Generator[+T] {
  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
    override def generate = f(Generator.this.generate)
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    override def generate = f(Generator.this.generate).generate
  }

  def single[T](x: T): Generator[T] = new Generator[T] {
    override def generate = x
  }

  def choose(lo: Int, hi: Int): Generator[Int] = for (x <- integers) yield lo + x & (hi - lo)


  def oneOf[T](xs: T*): Generator[T] = for (idx <- choose(0, xs.length)) yield xs(idx)

  def lists: Generator[List[T]] = {
    def emptyList = single(Nil)
    def nonEmptyList = for {
      head <- this
      tail <- lists
    } yield (head :: tail)

    for {
      isEmpty <- booleans
      list <- if (isEmpty) emptyList else nonEmptyList
    } yield list
  }

  def trees: Generator[Tree[T]] = {
    def leafs = for {x <- this } yield new Leaf(x)
    def inners = for {l <- trees; r <- trees} yield new Inner(l, r)

    for {
      isLeaf <- booleans
      tree <- if (isLeaf) leafs else inners
    } yield tree
  }
}

trait Tree[+T]
case class Inner[+T](left: Tree[T], right: Tree[T]) extends Tree[T]
case class Leaf[+T](x: T) extends Tree[T]




val pairs = new Generator[(Int, Int)] {
  override def generate = (integers.generate, integers.generate)
}

val booleans2 = for (x <- integers) yield x > 0

val intLists = integers.lists
val boolLists = booleans2.lists
val intTrees = integers.trees


booleans.generate
booleans.generate
booleans.generate


booleans2.generate
intLists.generate
intLists.generate
intLists.generate
intLists.generate

boolLists.generate
boolLists.generate
boolLists.generate
boolLists.generate

intTrees.generate
intTrees.generate
intTrees.generate