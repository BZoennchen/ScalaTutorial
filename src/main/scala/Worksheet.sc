class Vector(val x: Double, val y: Double) {
  def +(other: Vector) = new Vector(other.x + x, other.y + y)

  def -(other: Vector) = new Vector(other.x - x, other.y - y)

  def *(other: Vector) = new Vector(other.x * x, other.y * y)

  def *(scalar: Double) = new Vector(x * scalar, y * scalar)

  def /(scalar: Double) = new Vector(x / scalar, y / scalar)

  def length() = Math.sqrt(x * x + y * y)

  def distance(other: Vector) = (this - other).length

  def norm(): Vector = new Vector(x / length, y / length)

  override def toString: String = s"($x, $y)"
}

object Body {
  val gravityConstant = 6.674*10E-11
}

class Body(val mass: Double, val position: Vector, val velocity: Vector) {
  override def toString: String = return "position=" + position + ", velocity=" + velocity
  def move(delta: Vector): Body = new Body(mass, position + delta, velocity)
  def distance(other: Body): Double = other.position.distance(position)

  private[this] def partialForce(other: Body): Vector = {
    val forceDirection = (other.position - position).norm()
    val forceStrength = (Body.gravityConstant * (other.mass * mass) / (distance(other) * distance(other)))
    return forceDirection * forceStrength
  }

  def acceleration(bodies: Traversable[Body]): Vector = {
    return bodies.map(body => partialForce(body)).reduce((a, b) => a + b) / mass

  }
}

class System(val bodies: Traversable[Body]) {
  def update(seconds: Double): System = {
    new System(bodies map (body => (body move body.acceleration(bodies) * seconds)))
  }
}

var planet1 = new Body(4.0, position = new Vector(1.1, 1.1), velocity = new Vector(1.0, 2.0))
var planet2 = new Body(5.0, position = new Vector(3.2, 4.1), velocity = new Vector(1.0, 2.0))
var planet3 = new Body(5.0, position = new Vector(1.2, 1.1), velocity = new Vector(1.0, 2.0))

planet1.distance(planet2)
planet1.distance(planet1)
planet2.distance(planet1)
planet1.acceleration(List(planet2, planet3))