package hackerrank

/**
  * Created by bzoennchen on 09.08.16.
  */
  object FunctionTest {

    def main(args: Array[String]) {
      //val lines = io.Source.stdin.getLines().toList
      val lines = List("3","3","1 2", "1 5", "4 5", "2", "1 2", "1 3", "3", "1 2", "1 2", "2 4")
      var line = 1

      for(i <- 1 to lines(0).toInt) {
        val numberOfPairs = lines(line).toInt
        val map3 = scala.collection.mutable.HashMap.empty[String, List[String]]
        line += 1
        val pairs = lines.take(line+numberOfPairs).drop(line).map(l => l.split(" ")).foreach(x => {
          if(map3.contains(x(0))) {
            map3(x(0)) = (x(1) :: map3(x(0)))
          } else {
            map3(x(0)) = List(x(1))
          }
        })
        line += numberOfPairs
        if(map3.values.map(list => list.distinct.length > 1).reduce(_ || _)) {
          println("NO")
        }
        else {
          println("YES")
        }
      }
    }
  }
