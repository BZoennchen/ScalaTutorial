/**
  * Created by bzoennchen on 07.08.16.
  */
class System(val bodies: Traversable[Body]) extends Traversable[Body]{

  def update(seconds: Double, delta: Double): System = {
    val steps = (seconds / delta).toInt
    (1 to steps).foldLeft(this)((system, i) => system.update(delta))
  }

  def update(seconds: Double): System = {
    // move bodies
    val newBodies = bodies.map(body => (body accelerate body.acceleration(bodies) * seconds)).map(body => (body move (body.velocity * seconds)))

    // merge bodies
    return new System(newBodies)
  }

  override def foreach[U](f: (Body) => U): Unit = bodies.foreach(f)
}
