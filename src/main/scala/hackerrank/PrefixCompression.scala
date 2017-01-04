package hackerrank

/**
  * Created by bzoennchen on 09.12.16.
  */
object PrefixCompression {
  def main(args: Array[String]) {
    val lines = io.Source.stdin.getLines().toList
    val x = lines(0)
    val y = lines(1)

    val pref = commonPrefix(x, y)
    println(pref + " " + x.substring(0, pref))
    println(x.length-pref + " " + x.substring(pref))
    println(y.length-pref + " " + y.substring(pref))
  }

  def commonPrefix(x: String, y: String): Int = {
    val minLength = Math.min(x.length, y.length)
    for(i <- 0 to minLength-1) {
      if(x.charAt(i) != y.charAt(i)) {
        return i;
      }
    }
    return minLength
  }
}
