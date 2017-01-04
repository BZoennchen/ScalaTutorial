import processing.core.PApplet

import scala.util.Random

object Test {
  def main(args: Array[String]): Unit = {
    PApplet.main("Test")
    io.Source.stdin.getLines()
  }
}

class Test extends PApplet {
  val steps = 10
  val step = 0
  val numberOfPoints = 100
  val framerate = 25
  var earthMass = 50
  var maxMass = 50
  var maxVelocity = 0.09
  val refreshCounter = 30
  var counter = 0

  var max = 0
  var system = new System(List())
  val systems = new Backup[System](10)


  override def settings() = {
    size(1000, 1000)
    max = 100
    val random = new Random()

    /*system = new System(1 to 5 map(i => new Body(
      random.nextDouble() * maxMass,
      new Vector(random.nextDouble() * width, random.nextDouble() * height),
      new Vector(random.nextDouble() * (if(random.nextBoolean()) 1 else -1) * maxVelocity, random.nextDouble() * (if(random.nextBoolean()) 1 else -1) * maxVelocity))))
*/
    system = new System(
      List(
        new Body(maxMass, new Vector(width / 2, height / 2), new Vector(0,0)),

        new Body(0.1 * maxMass, new Vector(width / 3, height / 3), new Vector(maxVelocity,0)),
        new Body(0.1 * maxMass, new Vector(width - width / 3, height - height / 3), new Vector(-maxVelocity,0)),
        new Body(0.1 * maxMass, new Vector(width / 3, height - height / 3), new Vector(0,-maxVelocity)),
        new Body(0.1 * maxMass, new Vector(width - width / 3, height / 3), new Vector(0,maxVelocity))
      )
    )

    systems.add(system)
    //system = new System(List(earth, venus, planet1, planet2, planet3))

    new Thread(new Runnable {
      override def run(): Unit = {
        while (true) {
          systems.synchronized {
            system = system.update(30, 0.1)
            systems.add(system)
          }
          Thread.sleep(40)
        }
      }
    }).start()
  }


  override def setup() = {}

  override def draw() = {

    /*if(counter==0) {
      background(0)
    }*/
    background(0)
    counter += 1


    noStroke()
    systems.synchronized {
      systems.list.foreach(system => system.foreach(body =>  ellipse(body.position.x.toInt, body.position.y.toInt, (body.mass).toInt, (body.mass).toInt)))
    }

    //ellipse(mouseX, mouseY, 20, 20)

  }
}