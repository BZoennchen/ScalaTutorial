class Vector(val x: Double, val y: Double) {
  def +(other: Vector) = new Vector(x + other.x, y + other.y)

  def -(other: Vector) = new Vector(x - other.x, y - other.y)

  def *(other: Vector) = new Vector(x * other.x, y * other.y)

  def *(scalar: Double) = new Vector(x * scalar, y * scalar)

  def /(scalar: Double) = new Vector(x / scalar, y / scalar)

  def length() = Math.sqrt(x * x + y * y)

  def distance(other: Vector) = (this - other).length

  def norm(): Vector = new Vector(x / length, y / length)

  override def toString: String = s"($x, $y)"
}
