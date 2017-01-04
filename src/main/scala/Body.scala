/**
  * Created by bzoennchen on 07.08.16.
  */
object Body {
  val gravityConstant = 0.05
}

class Body(val mass: Double, val position: Vector, val velocity: Vector) {
  override def toString: String = return "position=" + position + ", velocity=" + velocity
  def move(delta: Vector): Body = new Body(mass, position + delta, velocity)
  def accelerate(deltaVelocity: Vector): Body = new Body(mass, position, velocity + deltaVelocity)
  def distance(other: Body): Double = other.position.distance(position)

  private[this] def partialForce(other: Body): Vector = {
    val forceDirection = (other.position - position).norm()
    val forceStrength = (Body.gravityConstant * (other.mass * mass) / (distance(other) * distance(other)))
    return forceDirection * forceStrength
  }

  def acceleration(bodies: Traversable[Body]): Vector = {
    return bodies.filter(body => body != this).map(body => partialForce(body)).reduce((a, b) => a + b) / mass

  }
}
