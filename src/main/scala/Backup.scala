/**
  * Created by bzoennchen on 07.08.16.
  */
class Backup[T](size: Int) {
  var list = new scala.collection.mutable.Queue[T]
  def add(element: T): Unit = {
    list += element
    if(list.size > size) {
      list.dequeue()
    }
  }
}
