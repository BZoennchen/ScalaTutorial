package hackerrank

import scala.annotation.tailrec


/**
  * Created by bzoennchen on 25.08.16.
  */
object StringCompression {

  class CChar(val c: Char, val count: Int) {
    def toCharArray(): Array[Char] = {
      (c, count) match {
        case (_, 1) => Array(c)
        case (_, _) => {
          val tmp = count.toString.toCharArray
          val arr = new Array[Char](tmp.size+1)
          System.arraycopy(tmp, 0, arr, 1, tmp.length)
          arr(0) = c
          arr
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val msg = io.StdIn.readLine()
    compress(msg).map(cchar => cchar.toCharArray()).foreach(arr => arr.foreach(c => print(c)))
  }

  def compress(msg: String): List[CChar] = {
    @tailrec
    def compressAcc(msg : String, acc: List[CChar]): List[CChar] = {
      val nextMsg = msg.dropWhile(c => msg.charAt(0) == c)
      val cchar = new CChar(msg.charAt(0), msg.length - nextMsg.length)
      nextMsg match {
        case "" => cchar :: acc
        case _ => compressAcc(nextMsg, cchar :: acc)
      }
    }
    compressAcc(msg, List()).reverse
  }
}
